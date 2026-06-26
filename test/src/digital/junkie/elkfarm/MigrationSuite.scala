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
import io.circe.parser.parse
import munit.CatsEffectSuite
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.{Method, Request, Uri}

import scala.util.Random

/** End-to-end migration test. Assumes an ElasticSearch instance is reachable at
  * http://localhost:9200 with no authentication.
  *
  * NOTE: this test is expected to FAIL for now: the step that invokes the
  * elkfarm tool to perform the migration is not yet wired up, so the alias is
  * never switched to the new index and the verification below fails.
  */
class MigrationSuite extends CatsEffectSuite {

  private val esUrl = "http://localhost:9200"

  /** A short, lowercase, random index-name suffix: `_` + 5 letters. */
  private def randomSuffix: String =
    "_" + Random.alphanumeric.filter(_.isLetter).take(5).mkString.toLowerCase

  private def loadMapping(name: String): Json = {
    // Mill runs tests in a sandbox working dir and exposes the test resource
    // folder via MILL_TEST_RESOURCE_DIR; fall back to the on-disk layout.
    val dir = sys.env.getOrElse("MILL_TEST_RESOURCE_DIR", "test/resources")
    val src = scala.io.Source.fromFile(s"$dir/mappings/$name")
    try parse(src.mkString).fold(throw _, identity)
    finally src.close()
  }

  test("simple migration switches the alias and copies docs to the new index") {
    val base   = "elkfarm_test" + randomSuffix
    val alias  = base
    val v1     = s"${base}_v1"
    val v2     = s"${base}_v2"
    val before = loadMapping("before.json")
    val after  = loadMapping("after.json")

    // sanity: both fixtures are present and parse as JSON objects
    assert(before.isObject, "before.json should be a JSON object")
    assert(after.isObject, "after.json should be a JSON object")

    val docs = List(
      Json.obj("name" -> Json.fromString("Widget"), "price" -> Json.fromInt(10)),
      Json.obj("name" -> Json.fromString("Gadget"), "price" -> Json.fromInt(20)),
      Json.obj("name" -> Json.fromString("Gizmo"),  "price" -> Json.fromInt(30))
    )

    EmberClientBuilder.default[IO].build.use { client =>
      def uri(path: String): Uri = Uri.unsafeFromString(s"$esUrl$path")

      def createIndex(index: String, body: Json): IO[Unit] =
        client
          .expect[Json](Request[IO](Method.PUT, uri(s"/$index")).withEntity(body))
          .void

      def addAlias(name: String, index: String): IO[Unit] =
        client
          .expect[Json](
            Request[IO](Method.POST, uri("/_aliases")).withEntity(
              Json.obj(
                "actions" -> Json.arr(
                  Json.obj(
                    "add" -> Json.obj(
                      "index" -> Json.fromString(index),
                      "alias" -> Json.fromString(name)
                    )
                  )
                )
              )
            )
          )
          .void

      def indexDoc(index: String, id: Int, doc: Json): IO[Unit] =
        client
          .expect[Json](
            Request[IO](Method.PUT, uri(s"/$index/_doc/$id?refresh=true"))
              .withEntity(doc)
          )
          .void

      def refresh(index: String): IO[Unit] =
        client
          .expect[Json](Request[IO](Method.POST, uri(s"/$index/_refresh")))
          .void

      def countDocs(index: String): IO[Long] =
        client
          .expect[Json](Request[IO](Method.GET, uri(s"/$index/_count")))
          .map(_.hcursor.get[Long]("count").getOrElse(0L))

      def deleteIndex(index: String): IO[Unit] =
        client.status(Request[IO](Method.DELETE, uri(s"/$index"))).attempt.void

      val seedDocs =
        docs.zipWithIndex.foldLeft(IO.unit) { case (acc, (doc, i)) =>
          acc.flatMap(_ => indexDoc(v1, i + 1, doc))
        }

      val run =
        for {
          // Arrange: a "before" index with an alias and a few documents.
          _ <- createIndex(v1, before)
          _ <- addAlias(alias, v1)
          _ <- seedDocs

          // Act: run the elkfarm migration into the new index with the new mapping.
          _ <- Migration.run(
            client = client,
            url = esUrl,
            source = v1,
            dest = v2,
            alias = alias,
            mapping = after
          )

          // Assert: the alias now points at the new index and the docs moved.
          state <- Elastic.listIndicesAndAliases[IO](client, esUrl)
          target = state.aliases.find(_.alias == alias).map(_.index)
          _ = assertEquals(target, Some(v2), "alias should point at the new index")
          _        <- refresh(v2)
          migrated <- countDocs(v2)
          _ = assertEquals(
            migrated,
            docs.size.toLong,
            "all docs should be reindexed into the new index"
          )
        } yield ()

      run.guarantee(deleteIndex(v1).flatMap(_ => deleteIndex(v2)))
    }
  }
}
