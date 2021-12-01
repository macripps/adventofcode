ThisBuild / organization := "com.github/macripps"
ThisBuild / version := "0.1-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "Advent Of Code",
    libraryDependencies += "com.twitter" %% "util-app" % "21.10.0",
    libraryDependencies += "io.opentelemetry" % "opentelemetry-api" % "1.9.0",
    libraryDependencies += "io.opentelemetry" % "opentelemetry-sdk" % "1.9.0",
    libraryDependencies += "io.opentelemetry" % "opentelemetry-exporter-prometheus" % "0.13.1",
    libraryDependencies += "io.opentelemetry" % "opentelemetry-exporter-zipkin" % "1.9.0",
    libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  )

scalacOptions ++= Seq("-deprecation", "-feature")
