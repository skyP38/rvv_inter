ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.3.1" // или "2.13.12" для Scala 2

lazy val root = (project in file("."))
  .settings(
    name := "lambda-calc",
    Compile / mainClass := Some("lambda.run"),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "3.0.2", // для парсера
      "org.scalatest" %% "scalatest" % "3.2.16" % Test // для тестов
    )
  )

