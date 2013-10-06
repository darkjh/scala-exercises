import sbt._
import Keys._

object ScalaExerciseBuild extends Build {

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    // project settings
    version := "0.1.0-SNAPSHOT",
    organization := "me.juhanlol",
    scalaVersion := "2.10.2",
    // dependencies
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "1.9.2" % "test",
      "org.scala-lang" % "scala-swing" % "2.10.2")
  )

  lazy val project = Project("scala-exercises", file("."),
    settings = buildSettings)
}