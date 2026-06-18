package digital.junkie.elkfarm

import cats.effect.Async
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.io.net.Network
import io.circe.{Decoder, Json}
import io.circe.generic.semiauto.deriveDecoder
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.{Method, Request, Uri}

object Elastic {

  object json {

    final case class CatIndex(
        index: String,
        health: Option[String],
        status: Option[String],
        uuid: Option[String],
        pri: Option[String],
        rep: Option[String],
        `docs.count`: Option[String],
        `docs.deleted`: Option[String],
        `store.size`: Option[String],
        `pri.store.size`: Option[String]
    )

    object CatIndex {
      implicit val decoder: Decoder[CatIndex] = deriveDecoder
    }

    final case class CatAlias(
        alias: String,
        index: String,
        filter: Option[String],
        `routing.index`: Option[String],
        `routing.search`: Option[String],
        is_write_index: Option[String]
    )

    object CatAlias {
      implicit val decoder: Decoder[CatAlias] = deriveDecoder
    }
  }

  case class State(indices: Seq[json.CatIndex], aliases: Seq[json.CatAlias])

  /** A snapshot of a reindex task: whether it has finished and how many
    * documents have been processed so far out of the total.
    */
  case class TaskStatus(
      completed: Boolean,
      total: Long,
      created: Long,
      updated: Long,
      deleted: Long
  ) {
    def processed: Long = created + updated + deleted
  }

  def listIndicesAndAliases[F[_]: Async: Network: Console](
      url: String
  ): F[State] = {
    EmberClientBuilder.default[F].build.use { client =>
      for {
        base <- Async[F].fromEither(Uri.fromString(url))
        indicesReq = Request[F](
          Method.GET,
          (base / "_cat" / "indices").withQueryParam("format", "json")
        )
        indices <- client.expect[List[json.CatIndex]](indicesReq)
        aliasesReq = Request[F](
          Method.GET,
          (base / "_cat" / "aliases").withQueryParam("format", "json")
        )
        aliases <- client.expect[List[json.CatAlias]](aliasesReq)
      } yield State(indices, aliases)
    }
  }

  def getMapping[F[_]: Async: Network: Console](
      url: String,
      index: String
  ): F[Json] = {
    EmberClientBuilder.default[F].build.use { client =>
      for {
        base <- Async[F].fromEither(Uri.fromString(url))
        req = Request[F](Method.GET, base / index / "_mapping")
        mapping <- client.expect[Json](req)
      } yield mapping
    }
  }

  def createIndex[F[_]: Async: Network: Console](
      url: String,
      index: String,
      body: Json
  ): F[Json] = {
    EmberClientBuilder.default[F].build.use { client =>
      for {
        base <- Async[F].fromEither(Uri.fromString(url))
        req = Request[F](Method.PUT, base / index).withEntity(body)
        resp <- client.expect[Json](req)
      } yield resp
    }
  }

  def aliasSwitch[F[_]: Async: Network: Console](
      url: String,
      alias: String,
      oldIndex: String,
      newIndex: String
  ): F[Json] = {
    val body = Json.obj(
      "actions" -> Json.arr(
        Json.obj(
          "remove" -> Json.obj(
            "index" -> Json.fromString(oldIndex),
            "alias" -> Json.fromString(alias)
          )
        ),
        Json.obj(
          "add" -> Json.obj(
            "index" -> Json.fromString(newIndex),
            "alias" -> Json.fromString(alias)
          )
        )
      )
    )
    EmberClientBuilder.default[F].build.use { client =>
      for {
        base <- Async[F].fromEither(Uri.fromString(url))
        req = Request[F](Method.POST, base / "_aliases").withEntity(body)
        resp <- client.expect[Json](req)
      } yield resp
    }
  }

  /** Starts an asynchronous reindex from `source` into `dest` and returns the
    * task id immediately (`wait_for_completion=false`), without waiting for it
    * to finish. Poll progress with [[taskStatus]].
    */
  def reindex[F[_]: Async: Network: Console](
      url: String,
      source: String,
      dest: String
  ): F[String] = {
    val body = Json.obj(
      "source" -> Json.obj("index" -> Json.fromString(source)),
      "dest"   -> Json.obj("index" -> Json.fromString(dest))
    )
    EmberClientBuilder.default[F].build.use { client =>
      for {
        base <- Async[F].fromEither(Uri.fromString(url))
        uri = (base / "_reindex")
          .withQueryParam("wait_for_completion", "false")
        req = Request[F](Method.POST, uri).withEntity(body)
        resp <- client.expect[Json](req)
        taskId <- Async[F].fromEither(
          resp.hcursor
            .get[String]("task")
            .leftMap(f => new RuntimeException(s"No task id in response: $f"))
        )
      } yield taskId
    }
  }

  /** Looks up the current state of a reindex task by id, reporting whether it
    * has completed and how many documents have been processed.
    */
  def taskStatus[F[_]: Async: Network: Console](
      url: String,
      taskId: String
  ): F[TaskStatus] = {
    EmberClientBuilder.default[F].build.use { client =>
      for {
        base <- Async[F].fromEither(Uri.fromString(url))
        req = Request[F](Method.GET, base / "_tasks" / taskId)
        resp <- client.expect[Json](req)
        status <- Async[F].fromEither(
          decodeTaskStatus(resp)
            .leftMap(f => new RuntimeException(s"Could not read task $taskId: $f"))
        )
      } yield status
    }
  }

  private def decodeTaskStatus(json: Json): Either[io.circe.DecodingFailure, TaskStatus] = {
    val root   = json.hcursor
    val status = root.downField("task").downField("status")
    for {
      completed <- root.get[Boolean]("completed")
      total     <- status.getOrElse[Long]("total")(0L)
      created   <- status.getOrElse[Long]("created")(0L)
      updated   <- status.getOrElse[Long]("updated")(0L)
      deleted   <- status.getOrElse[Long]("deleted")(0L)
    } yield TaskStatus(completed, total, created, updated, deleted)
  }
}
