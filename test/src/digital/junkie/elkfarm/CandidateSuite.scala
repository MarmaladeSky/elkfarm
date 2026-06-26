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

import digital.junkie.elkfarm.Elastic.State
import munit.CatsEffectSuite

/** Unit tests (no Elasticsearch required) for picking the index to migrate. The
  * key case is the one that used to crash: an interactive run against a cluster
  * with no managed `<alias>_v<n>` indices must abort cleanly rather than throw
  * out of the empty selection menu.
  */
class CandidateSuite extends CatsEffectSuite {

  private def index(name: String): Elastic.json.CatIndex =
    Elastic.json
      .CatIndex(name, None, None, None, None, None, None, None, None, None)

  private def alias(name: String, idx: String): Elastic.json.CatAlias =
    Elastic.json.CatAlias(name, idx, None, None, None, None)

  test("findCandidates is empty when no alias points to a versioned index") {
    val state = State(
      indices = Seq(index("orders_v1"), index("logs")),
      aliases = Seq(alias("logs", "logs")) // target has no _v<n> suffix
    )
    assertEquals(Main.findCandidates(state), Seq.empty)
  }

  test("findCandidates picks the single versioned index an alias points to") {
    val state = State(
      indices = Seq(index("orders_v1"), index("orders_v2"), index("orders_v3")),
      aliases = Seq(alias("orders", "orders_v2"))
    )
    assertEquals(
      Main.findCandidates(state),
      Seq(ManagedIndex("orders", 2, Seq(1, 2, 3)))
    )
  }

  test("findCandidates ignores aliases pointing to more than one index") {
    val state = State(
      indices = Seq(index("orders_v1"), index("orders_v2")),
      aliases = Seq(alias("orders", "orders_v1"), alias("orders", "orders_v2"))
    )
    assertEquals(Main.findCandidates(state), Seq.empty)
  }

  test("selectIndex aborts cleanly with no candidates (interactive path)") {
    interceptIO[Main.Abort](Main.selectIndex(Seq.empty, None))
      .map(e => assert(e.getMessage.contains("No managed indices")))
  }

  test("selectIndex aborts with no candidates even when --alias was given") {
    interceptIO[Main.Abort](Main.selectIndex(Seq.empty, Some("orders")))
      .map(e => assert(e.getMessage.contains("No managed indices")))
  }

  test("selectIndex returns the matching candidate for a known --alias") {
    val orders = ManagedIndex("orders", 2, Seq(1, 2))
    Main.selectIndex(Seq(orders), Some("orders")).assertEquals(orders)
  }

  test("selectIndex aborts for an unknown --alias") {
    val orders = ManagedIndex("orders", 2, Seq(1, 2))
    interceptIO[Main.Abort](Main.selectIndex(Seq(orders), Some("nope")))
      .map(e => assert(e.getMessage.contains("not found")))
  }
}
