ThisBuild / organization := "com.github/macripps"
ThisBuild / version := "0.1-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.4"

lazy val root = (project in file("."))
  .settings(
    name := "Advent Of Code",
    libraryDependencies += "com.twitter" %% "util-app" % "20.10.0",
    libraryDependencies += "io.opentelemetry" % "opentelemetry-api" % "0.12.0",
    libraryDependencies += "io.opentelemetry" % "opentelemetry-sdk" % "0.12.0",
    libraryDependencies += "io.opentelemetry" % "opentelemetry-exporter-prometheus" % "0.12.0",
    libraryDependencies += "io.opentelemetry" % "opentelemetry-exporter-zipkin" % "0.12.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % Test,
  )

scalacOptions ++= Seq("-deprecation", "-feature")
