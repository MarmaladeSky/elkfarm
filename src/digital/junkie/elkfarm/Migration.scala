/*
 * Copyright 2026 David Akermann
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 */

package digital.junkie.elkfarm

import cats.effect.IO
import io.circe.Json
import org.http4s.client.Client

import scala.concurrent.duration.*

object Migration {

  /** Raised when the reindex finishes with per-document failures or a top-level
    * task error. Carries the details so the caller can report them; the alias
    * is never switched in this case.
    */
  final case class ReindexFailed(failures: Vector[Json], error: Option[Json])
      extends RuntimeException("reindex failed") {
    override def fillInStackTrace(): Throwable = this
  }

  /** Executes a simple migration: create the destination index with the given
    * mapping, reindex documents from the source index, wait for the reindex to
    * finish, then switch the alias from the source to the destination index.
    */
  def run(
      client: Client[IO],
      url: String,
      source: String,
      dest: String,
      alias: String,
      mapping: Json
  ): IO[Unit] =
    for {
      _ <- Spinner(s"Creating index $dest")(
        Elastic.createIndex[IO](client, url, dest, mapping)
      )
      task <- Spinner(s"Starting reindex $source -> $dest")(
        Elastic.reindex[IO](client, url, source, dest)
      )
      _ <- awaitTask(client, url, task)
      _ <- Spinner(s"Switching alias $alias to $dest")(
        Elastic.aliasSwitch[IO](
          client,
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
  private def awaitTask(
      client: Client[IO],
      url: String,
      taskId: String
  ): IO[Unit] = {
    IO.ref(0L -> 0L).flatMap { progress =>
      // Poll the task every second, recording the latest doc counts so the
      // spinner can render live progress; the pulse bar keeps animating between
      // polls while the numbers update. Returns the final status on completion.
      def poll: IO[Elastic.TaskStatus] =
        Elastic.taskStatus[IO](client, url, taskId).flatMap { status =>
          progress.set(status.processed -> status.total) >>
            (if (status.completed) IO.pure(status)
             else IO.sleep(1.second) >> poll)
        }

      val label = progress.get.map { case (processed, total) =>
        s"reindex (task $taskId): $processed/$total docs processed"
      }

      Spinner.withProgress(label)(poll).flatMap { status =>
        if (status.failed)
          IO.raiseError(ReindexFailed(status.failures, status.error))
        else label.flatMap(l => IO.println(s"$l — done"))
      }
    }
  }
}
