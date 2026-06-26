package digital.junkie.elkfarm

import cats.data.{Validated, ValidatedNel}
import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import cats.syntax.foldable.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import digital.junkie.elkfarm.Elastic.State
import io.circe.Json
import io.circe.parser.parse
import org.http4s.client.Client

import java.io.File

object Main
    extends CommandIOApp(
      name = "elkfarm",
      header = "Manage ElasticSearch indices via aliases"
    ) {

  val urlOpt = Opts
    .option[String]("url", "ElasticSearch URL", short = "u")
    .orNone

  val mappingsOpt = Opts
    .option[String]("mappings", "Path to the mappings file", short = "m")
    .orNone

  val aliasOpt = Opts
    .option[String]("alias", "Alias to manage", short = "a")
    .orNone

  val workflowOpt = Opts
    .option[String]("workflow", "Workflow to run", short = "w")
    .orNone

  val yesOpt: Opts[Boolean] = Opts
    .flag("yes", "Skip the confirmation and apply the changes", short = "y")
    .orFalse

  sealed trait Prune
  case object PruneAll               extends Prune
  final case class PruneKeep(n: Int) extends Prune

  def parsePrune(raw: String): ValidatedNel[String, Prune] =
    raw.trim.toLowerCase match {
      case "all" => Validated.valid(PruneAll)
      case other =>
        other.toIntOption match {
          case Some(n) if n >= 0 => Validated.valid(PruneKeep(n))
          case _ =>
            Validated.invalidNel(
              s"--prune expects 'all' or a non-negative number, got '$raw'"
            )
        }
    }

  val pruneOpt: Opts[Option[Prune]] = Opts
    .option[String](
      "prune",
      "Delete previous index versions: 'all', or N to keep the latest N",
      short = "p"
    )
    .mapValidated(parsePrune)
    .orNone

  final class Abort(message: String) extends RuntimeException(message) {
    override def fillInStackTrace(): Throwable = this
  }

  def main: Opts[IO[ExitCode]] = {
    (urlOpt, mappingsOpt, aliasOpt, workflowOpt, yesOpt, pruneOpt).mapN {
      (url, mappings, alias, workflow, yes, prune) =>
        execution(url, mappings, alias, workflow, yes, prune)
          .as(ExitCode.Success)
          .recover { case Menu.Interrupted => ExitCode.Success }
          .recoverWith { case e: Abort =>
            IO.println(e.getMessage).as(ExitCode.Error)
          }
    }
  }

  val VersionSuffix = "_v(\\d+)".r

  def versionOf(aliasName: String, index: String): Option[Int] = {
    if (index.startsWith(aliasName))
      index.substring(aliasName.length) match {
        case VersionSuffix(v) => Some(v.toInt)
        case _                => None
      }
    else None
  }

  def findCandidates(state: State): Seq[ManagedIndex] = {

    state.aliases
      .groupBy(_.alias)
      .flatMap { case (name, aliases) =>
        val existingIndices =
          state.indices.flatMap(i => versionOf(name, i.index))

        if (aliases.size == 1)
          versionOf(name, aliases.head.index)
            .map(current => ManagedIndex(name, current, existingIndices))
        else None
      }
      .toSeq
  }

  def selectIndex(
      candidates: Seq[ManagedIndex],
      aliasArg: Option[String]
  ): IO[ManagedIndex] = {
    if (candidates.isEmpty)
      IO.raiseError(
        new Abort(
          "No managed indices found. Expected at least one alias that points " +
            "to a single index named '<alias>_v<number>'."
        )
      )
    else
      aliasArg match {
        case Some(a) =>
          candidates.find(_.name == a) match {
            case Some(c) => IO.pure(c)
            case None =>
              IO.raiseError(
                new Abort(s"Alias '$a' not found among managed aliases.")
              )
          }
        case None =>
          Menu.search[IO, ManagedIndex](
            candidates,
            title = "Select an index",
            show = _.name
          )
      }
  }

  def execution(
      urlArg: Option[String],
      mappingsArg: Option[String],
      aliasArg: Option[String],
      workflowArg: Option[String],
      assumeYes: Boolean,
      prune: Option[Prune]
  ): IO[Unit] = Elastic.clientResource[IO].use { client =>
    for {
      _ <- workflowArg match {
        case Some(w) =>
          Workflow.byName(w) match {
            case Some(m) => IO.pure(m)
            case None =>
              IO.raiseError(
                new Abort(
                  s"Workflow '$w' not found. Available: ${Workflow.all.map(_.title).mkString(", ")}"
                )
              )
          }
        case None =>
          Menu.select[IO, Workflow](
            options = Workflow.all,
            title = "Select indices management",
            show = m => s"${m.title}: ${m.description}"
          )
      }
      esUrl <- urlArg match {
        case Some(u) => IO.pure(u)
        case None    => Menu.input[IO]("Please input ElasticSearch URL")
      }
      state <- Spinner("Fetching indices and aliases")(
        Elastic.listIndicesAndAliases[IO](client, esUrl)
      )
      candidates = findCandidates(state)
      index <- selectIndex(candidates, aliasArg)
      currentIndexName = index.currentIndexName
      newVersion <- Menu.input[IO](
        s"Version for the new index (current $currentIndexName)",
        default = index.nextIndexVersion.toString
      )
      nextIndexName = s"${index.name}_v$newVersion"
      mappingsFile <- mappingsArg match {
        case Some(path) => IO.pure(new File(path))
        case None =>
          Menu.searchFiles[IO](
            title = "Select mappings",
            start = "./"
          )
      }
      fileMapping <- readJsonFile(mappingsFile)
      _ <- printPlan(
        url = esUrl,
        alias = index.name,
        newIndex = nextIndexName,
        source = index.currentIndexName,
        mapping = fileMapping
      )
      execute <-
        if (assumeYes) IO.pure(true)
        else
          Menu.yesNo[IO]("Should execute the plan?")
      _ <- IO.raiseWhen(!execute) { Menu.Interrupted }
      _ <- Migration
        .run(
          client = client,
          url = esUrl,
          source = index.currentIndexName,
          dest = nextIndexName,
          alias = index.name,
          mapping = fileMapping
        )
        .recoverWith { case Migration.ReindexFailed(failures, error) =>
          handleReindexFailure(
            client,
            esUrl,
            nextIndexName,
            failures,
            error,
            assumeYes
          )
        }
      oldIndices = index.existingIndices.distinct.sorted
        .map(v => s"${index.name}_v$v")
        .filterNot(_ == nextIndexName)
      _ <- cleanup(client, esUrl, oldIndices, prune, assumeYes)
    } yield ()
  }

  def cleanup(
      client: Client[IO],
      url: String,
      oldIndices: Seq[String],
      prune: Option[Prune],
      assumeYes: Boolean
  ): IO[Unit] = {
    val byVersionDesc = oldIndices.sortBy(versionSuffix).reverse

    def planned(p: Prune): Seq[String] = p match {
      case PruneAll     => byVersionDesc
      case PruneKeep(n) => byVersionDesc.drop(n)
    }

    def delete(targets: Seq[String]): IO[Unit] = if (targets.isEmpty) {
      IO.println("Nothing to delete.")
    } else {
      targets.traverse_ { i =>
        Spinner(s"Deleting $i")(Elastic.deleteIndex[IO](client, url, i))
      }
    }

    if (oldIndices.isEmpty) {
      IO.println("No previous versions to clean up.")
    } else {
      (assumeYes, prune) match {
        case (true, None) =>
          IO.println("Skipping previous version deletion (no --prune given).")
        case (true, Some(p)) =>
          delete(planned(p))
        case (false, None) =>
          Menu
            .multiSelect[IO, String](
              options = byVersionDesc, // newest-first for display
              title = "Select previous versions to delete (Space to toggle)"
            )
            .flatMap(delete)
        case (false, Some(p)) =>
          val targets = planned(p)
          if (targets.isEmpty) {
            IO.println("Nothing to delete.")
          } else {
            for {
              _ <- IO.println(
                s"Will delete:\n${targets.map(i => s"  $i").mkString("\n")}"
              )
              executeDeletion <- Menu.yesNo[IO]("Delete these indices?")
              _               <- IO.whenA(executeDeletion)(delete(targets))
            } yield ()
          }
      }
    }
  }

  val VersionSuffixNum = ".*_v(\\d+)$".r

  def versionSuffix(index: String): Int = index match {
    case VersionSuffixNum(v) => v.toInt
    case _                   => Int.MinValue
  }

  def printPlan(
      url: String,
      alias: String,
      newIndex: String,
      source: String,
      mapping: Json
  ): IO[Unit] = {
    val reindexBody = Json.obj(
      "source" -> Json.obj("index" -> Json.fromString(source)),
      "dest"   -> Json.obj("index" -> Json.fromString(newIndex))
    )

    val aliasBody = Json.obj(
      "actions" -> Json.arr(
        Json.obj(
          "remove" -> Json.obj(
            "index" -> Json.fromString(source),
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

    def call(method: String, path: String, body: Json): String =
      s"$method $url$path\n${body.noSpaces}"

    IO.println(
      s"""|Planned API calls:
          |${call("PUT", s"/$newIndex", mapping)}
          |
          |${call("POST", "/_reindex?wait_for_completion=false", reindexBody)}
          |
          |${call("POST", "/_aliases", aliasBody)}
          |""".stripMargin
    )
  }

  def handleReindexFailure(
      client: Client[IO],
      url: String,
      dest: String,
      failures: Vector[Json],
      error: Option[Json],
      assumeYes: Boolean
  ): IO[Unit] = {
    for {
      _ <- IO.println(renderFailures(dest, failures, error))
      _ <- IO.whenA(!assumeYes) {
        for {
          del <- Menu.yesNo[IO](s"Delete the incomplete index $dest?")
          _ <- IO.whenA(del) {
            Spinner(s"Deleting $dest")(
              Elastic.deleteIndex[IO](client, url, dest)
            ).void
          }
        } yield ()
      }
      _ <- IO.raiseError(new Abort("Reindex failed; alias not switched."))
    } yield ()
  }

  def renderFailures(
      dest: String,
      failures: Vector[Json],
      error: Option[Json]
  ): String = {
    val Max = 10

    def reasonOf(cursor: io.circe.ACursor): Option[String] =
      cursor.get[String]("reason").toOption

    val errorLine = error.map { e =>
      val reason = reasonOf(e.hcursor).getOrElse(e.noSpaces)
      s"Task error: $reason"
    }

    val failureLines = failures.take(Max).map { f =>
      val c     = f.hcursor
      val id    = c.get[String]("id").toOption.getOrElse("?")
      val cause = c.downField("cause")
      val tpe   = cause.get[String]("type").toOption
      val reason = reasonOf(cause)
        .orElse(reasonOf(c))
        .getOrElse(f.noSpaces)
      val detail = tpe.fold(reason)(t => s"$t: $reason")
      s"  - id=$id: $detail"
    }

    val more =
      if (failures.size > Max) Seq(s"  (+${failures.size - Max} more)")
      else Seq.empty

    val countLine =
      if (failures.nonEmpty) Some(s"${failures.size} document failure(s):")
      else None

    (Seq(s"Reindex into $dest failed. The alias was NOT switched.") ++
      errorLine.toSeq ++
      countLine.toSeq ++
      failureLines ++
      more).mkString("\n")
  }

  def readJsonFile(file: File): IO[Json] = {
    IO.blocking {
      val src = scala.io.Source.fromFile(file)
      try src.mkString
      finally src.close()
    }.flatMap(content => IO.fromEither(parse(content)))
  }

}
