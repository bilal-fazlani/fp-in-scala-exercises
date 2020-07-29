import sbt._

lazy val root = project
  .in(file("."))
  .settings(
    name := "fp-in-scala",
    version := "0.1.0",
    scalaVersion := "2.13.3",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.9" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
