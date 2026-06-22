package digital.junkie.elkfarm

import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import digital.junkie.elkfarm.Elastic.State
import io.circe.Json
import io.circe.parser.parse

import java.io.File

object Main
    extends CommandIOApp(
      name = "elkfarm",
      header = "Manage ElasticSearch indices via aliases"
    ) {

  private val urlOpt = Opts
    .option[String]("url", "ElasticSearch URL", short = "u")
    .orNone

  private val mappingsOpt = Opts
    .option[String]("mappings", "Path to the mappings file", short = "m")
    .orNone

  private val aliasOpt = Opts
    .option[String]("alias", "Alias to manage", short = "a")
    .orNone

  private val workflowOpt = Opts
    .option[String]("workflow", "Workflow to run", short = "w")
    .orNone

  private val yesOpt: Opts[Boolean] = Opts
    .flag("yes", "Skip the confirmation and apply the changes", short = "y")
    .orFalse

  private final class Abort(message: String) extends RuntimeException(message) {
    override def fillInStackTrace(): Throwable = this
  }

  def main: Opts[IO[ExitCode]] = {
    (urlOpt, mappingsOpt, aliasOpt, workflowOpt, yesOpt).mapN {
      (url, mappings, alias, workflow, yes) =>
        fetchExample(url, mappings, alias, workflow, yes)
          .as(ExitCode.Success)
          .recover { case Menu.Interrupted => ExitCode.Success }
          .recoverWith { case e: Abort =>
            IO.println(e.getMessage).as(ExitCode.Error)
          }
    }
  }

  private val VersionSuffix = "_v(\\d+)".r

  private def versionOf(aliasName: String, index: String): Option[Int] = {
    if (index.startsWith(aliasName))
      index.substring(aliasName.length) match {
        case VersionSuffix(v) => Some(v.toInt)
        case _                => None
      }
    else None
  }

  private def findCandidates(state: State): Seq[ManagedIndex] = {

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

  private def fetchExample(
      urlArg: Option[String],
      mappingsArg: Option[String],
      aliasArg: Option[String],
      workflowArg: Option[String],
      assumeYes: Boolean
  ): IO[Unit] = {
    for {
      workflow <- workflowArg match {
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
        Elastic.listIndicesAndAliases[IO](esUrl)
      )
      candidates = findCandidates(state)
      index <- aliasArg match {
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
      fileMapping  <- readJsonFile(mappingsFile)
      indexMapping <- Elastic.getMapping[IO](esUrl, index.currentIndexName)
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
          Menu.select[IO, Boolean](
            Seq(true, false),
            "Should execute the plan?",
            answer => if (answer) "Yes" else "No"
          )
      _ <- IO.raiseWhen(!execute) { Menu.Interrupted }
      _ <- Migration.run(
        url = esUrl,
        source = index.currentIndexName,
        dest = nextIndexName,
        alias = index.name,
        mapping = fileMapping
      )
      _ <- IO.println(s"File mapping:\n${fileMapping.noSpacesSortKeys}")
      _ <- IO.println(
        s"Index mapping (${index.currentIndexName}):\n${indexMapping.noSpacesSortKeys}"
      )
    } yield ()
  }

  private def printPlan(
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

  private def readJsonFile(file: File): IO[Json] =
    IO.blocking {
      val src = scala.io.Source.fromFile(file)
      try src.mkString
      finally src.close()
    }.flatMap(content => IO.fromEither(parse(content)))

}
