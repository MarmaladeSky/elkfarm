package digital.junkie.elkfarm

import cats.effect.IO
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
      _ <- Spinner(s"Creating index $dest")(
        Elastic.createIndex[IO](url, dest, mapping)
      )
      task <- Spinner(s"Starting reindex $source -> $dest")(
        Elastic.reindex[IO](url, source, dest)
      )
      _ <- awaitTask(url, task)
      _ <- Spinner(s"Switching alias $alias to $dest")(
        Elastic.aliasSwitch[IO](
          url,
          alias = alias,
          oldIndex = source,
          newIndex = dest
        )
      )
    } yield ()

  /** Polls a reindex task until it reports completion, printing progress as it
    * goes. A failed status lookup (HTTP/decoding error) propagates as a failed
    * `IO`, aborting the flow.
    */
  private def awaitTask(url: String, taskId: String): IO[Unit] = {
    IO.ref(0L -> 0L).flatMap { progress =>
      // Poll the task every second, recording the latest doc counts so the
      // spinner can render live progress; the pulse bar keeps animating between
      // polls while the numbers update.
      def poll: IO[Unit] =
        Elastic.taskStatus[IO](url, taskId).flatMap { status =>
          progress.set(status.processed -> status.total) >>
            IO.whenA(!status.completed)(IO.sleep(1.second) >> poll)
        }

      val label = progress.get.map { case (processed, total) =>
        s"reindex (task $taskId): $processed/$total docs processed"
      }

      Spinner.withProgress(label)(poll) >>
        label.flatMap(l => IO.println(s"$l — done"))
    }
  }
}
