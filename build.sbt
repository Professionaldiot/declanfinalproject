ThisBuild / version := "3.0.0-RELEASE"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "declanfinalproject"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.creativescala" %% "doodle" % "0.20.0"