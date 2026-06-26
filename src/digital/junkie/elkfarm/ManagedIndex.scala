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

case class ManagedIndex(
    name: String,
    currentIndex: Int,
    existingIndices: Seq[Int]
) {

  def currentIndexName: String = s"${name}_v$currentIndex"
  
  def nextIndexVersion: Int = existingIndices.toSet.+(currentIndex).max + 1

}
