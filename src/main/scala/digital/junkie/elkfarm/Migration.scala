package digital.junkie.elkfarm

import cats.effect.IO
import cats.syntax.apply.*
import io.circe.Json

import scala.concurrent.duration.*

object Migration {

  /** Executes a simple migration: create the destination index with the given
    * mapping, reindex documents from the source index, wait for the reindex to
    * finish, then switch the alias from the source to the destination index.
    */
  def run(
      url: String,
      source: String,
      dest: String,
      alias: String,
      mapping: Json
  ): IO[Unit] =
    for {
      _    <- Elastic.createIndex[IO](url, dest, mapping)
      task <- Elastic.reindex[IO](url, source, dest)
      _    <- awaitTask(url, task)
      _ <- Elastic.aliasSwitch[IO](
        url,
        alias = alias,
        oldIndex = source,
        newIndex = dest
      )
    } yield ()

  /** Polls a reindex task until it reports completion, printing progress as it
    * goes. A failed status lookup (HTTP/decoding error) propagates as a failed
    * `IO`, aborting the flow.
    */
  private def awaitTask(url: String, taskId: String): IO[Unit] = {
    Elastic.taskStatus[IO](url, taskId).flatMap { status =>
      val progress =
        s"reindex: ${status.processed}/${status.total} docs processed"

      if (status.completed) {
        IO.println(s"$progress — done")
      } else {
        IO.println(progress) >> IO.sleep(1.second) >> awaitTask(url, taskId)
      }
    }
  }
}
