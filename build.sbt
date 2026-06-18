scalaVersion := "3.3.4"

enablePlugins(ScalaNativePlugin)

scalacOptions += "-no-indent"

name         := "elkfarm"
organization := "com.example"
version      := "0.1.0"

libraryDependencies ++= Seq(
  "org.http4s"    %%% "http4s-ember-client" % "0.23.34",
  "org.http4s"    %%% "http4s-circe"        % "0.23.34",
  "org.typelevel" %%% "cats-effect"         % "3.7.0",
  "io.circe"      %%% "circe-core"          % "0.14.10",
  "io.circe"      %%% "circe-generic"       % "0.14.10",
  "io.circe"      %%% "circe-parser"        % "0.14.10",
  "com.monovore"  %%% "decline"             % "2.6.2",
  "com.monovore"  %%% "decline-effect"      % "2.6.2",
  "org.typelevel" %%% "munit-cats-effect"   % "2.2.0" % Test
)

testFrameworks += new TestFramework("munit.Framework")

import scala.scalanative.build._

nativeConfig ~= { c =>
  c.withMode(Mode.debug)
    .withLTO(LTO.none)
}
