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

import scala.concurrent.duration.*

object Spinner {

  private val esc = ""

  /** Pulse-bar frames: a five-cell block that fills then empties. */
  private val frames = Vector(
    "[▱▱▱▱▱]",
    "[▰▱▱▱▱]",
    "[▰▰▱▱▱]",
    "[▰▰▰▱▱]",
    "[▰▰▰▰▱]",
    "[▰▰▰▰▰]"
  )

  private val interval = 120.millis

  /** Runs `task`, showing an animated pulse bar with `label` on the current
    * line while it is in flight. The bar runs on a background fiber and is
    * cancelled once `task` finishes (or fails); the line is cleared and the
    * cursor restored on the way out.
    */
  def apply[A](label: String)(task: IO[A]): IO[A] =
    withProgress(IO.pure(label))(task)

  /** Like [[apply]], but `label` is re-read on every frame, so a caller can
    * surface live progress (e.g. a reindex doc count) next to the pulse bar
    * while the bar keeps animating in between updates.
    */
  def withProgress[A](label: IO[String])(task: IO[A]): IO[A] = {
    def draw(i: Int): IO[Unit] =
      label.flatMap { l =>
        IO(print(s"\r$esc[2K${frames(i % frames.length)} $l")) >>
          IO(Console.flush())
      }

    def loop(i: Int): IO[Unit] =
      draw(i) >> IO.sleep(interval) >> loop(i + 1)

    val hideCursor   = IO(print(s"$esc[?25l")) >> IO(Console.flush())
    val clearAndShow = IO(print(s"\r$esc[2K$esc[?25h")) >> IO(Console.flush())

    (hideCursor >> loop(0).background.surround(task)).guarantee(clearAndShow)
  }
}
