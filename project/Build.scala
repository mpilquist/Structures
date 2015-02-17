import sbt._
import Keys._

import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin._
import ScalaJSPlugin.autoImport._

object StructuresBuild extends Build {
  type PE = Project => Project

  lazy val addKindProjector =
    addCompilerPlugin("org.spire-math" % "kind-projector_2.11" % "0.5.2")

  lazy val commonSettings = Seq(
      organization := "com.github.mpilquist",
      scalaVersion := "2.11.5",
      crossScalaVersions := Seq("2.11.5"),
      scalacOptions ++= Seq(
        "-Xfatal-warnings",
        "-Ywarn-dead-code",
        "-Ywarn-numeric-widen",
        "-Ywarn-value-discard",
        "-encoding", "UTF-8",
        "-feature",
        "-language:higherKinds",
        "-language:implicitConversions",
        "-deprecation",
        "-unchecked",
        "-Xcheckinit",
        "-Xlint",
        "-Xverify",
        "-Xfuture",
        "-Yclosure-elim",
        "-Yinline",
        "-Yno-adapted-args"
      ),
      testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
      resolvers ++= Seq(
        "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/",
        "bintray/non" at "http://dl.bintray.com/non/maven"),
      initialCommands += """
        import structures._
        import structures.std._
      """
    )

  lazy val root = project.in(file(".")).aggregate(corejvm, laws, examples, corejs, jsExamples).settings(commonSettings: _*).settings(
    publishArtifact := false
  )

  lazy val core = crossProject.settings().
    settings(commonSettings: _*).
    settings(
      name := "structures-core",
      libraryDependencies ++= Seq(
        "com.github.mpilquist" %% "simulacrum" % "0.2.0" % "optional",
        "org.scalatest" %% "scalatest" % "2.2.3" % "test"
      ),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
      addKindProjector
    )

  lazy val corejvm = core.jvm
  lazy val corejs = core.js

  lazy val laws = project.dependsOn(corejvm).
    settings(commonSettings: _*).
    settings(
      name := "structures-laws",
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.11.3",
        "org.typelevel" %% "discipline" % "0.2.1",
        "org.scalatest" %% "scalatest" % "2.2.3" % "test"
      ),
      addKindProjector
    )

  lazy val examples = project.dependsOn(corejvm).
    settings(commonSettings: _*).
    settings(
      name := "structures-examples",
      addKindProjector
    )

  lazy val jsExamples = project.dependsOn(corejs).
    enablePlugins(ScalaJSPlugin).
    settings(commonSettings: _*).
    settings(
      name := "structures-js-examples",
      addKindProjector,
      libraryDependencies ++= Seq("org.scala-js" %%% "scalajs-dom" % "0.8.0")
    )

}




