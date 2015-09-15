import sbt._
import Keys._

object ScalaExerciseBuild extends Build {

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    // project settings
    version := "0.1.0-SNAPSHOT",
    organization := "me.juhanlol",
    scalaVersion := "2.11.7",
    // dependencies
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scala-lang" % "scala-swing" % "2.11.0-M7",
      "com.typesafe.akka" %% "akka-actor" % "2.3.13")
  )

  lazy val project = Project("scala-exercises", file("."),
    settings = buildSettings)
}