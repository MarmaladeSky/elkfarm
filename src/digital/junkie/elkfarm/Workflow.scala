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

sealed trait Workflow {
  def title: String
  def description: String
}

object Workflow {
  case object Simple extends Workflow {
    val title: String       = "Simple"
    val description: String = "A simple alias-based indices management"
  }

  val all: Seq[Workflow] = Seq(Simple)

  /** Looks up a workflow by its title, case-insensitively. */
  def byName(name: String): Option[Workflow] =
    all.find(_.title.equalsIgnoreCase(name))
}
