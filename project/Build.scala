import sbt._
import Keys._

object ScalaExerciseBuild extends Build {

  lazy val commonSettings = Seq(
    organization := "me.juhanlol",
    scalaVersion := "2.11.7"
  )

  lazy val depsSettings = Defaults.defaultSettings ++ Seq(
    // dependencies
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scala-lang" % "scala-swing" % "2.11.0-M7",
      "com.typesafe.akka" %% "akka-actor" % "2.3.13",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value)
  )

  lazy val exercises = (project in file("exercises"))
    .dependsOn(macros)
    .settings(commonSettings: _*)
    .settings(depsSettings: _*)

  lazy val macros = (project in file("macros"))
    .settings(commonSettings: _*)
    .settings(depsSettings: _*)
}