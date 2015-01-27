import sbt._
import Keys._

object StructuresBuild extends Build {

  lazy val commonSettings = Seq(
    organization := "com.github.mpilquist",
    scalaVersion := "2.11.5",
    crossScalaVersions := Seq("2.11.5"),
    scalacOptions := Seq(
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
    resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/",
    initialCommands += """
      import structures._
      import structures.instances._
    """
  )

  lazy val root = project.in(file(".")).aggregate(core, laws).settings(commonSettings: _*).settings(
    publishArtifact := false
  )

  lazy val core = project.settings().
    settings(commonSettings: _*).
    settings(
      name := "structures-core",
      libraryDependencies ++= Seq(
        "com.github.mpilquist" %% "simulacrum" % "0.1.0-SNAPSHOT" % "optional",
        "org.scalatest" %% "scalatest" % "2.2.3" % "test"
      ),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
    )

  lazy val laws = project.dependsOn(core).
    settings(commonSettings: _*).
    settings(
      name := "structures-laws",
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.11.3",
        "org.typelevel" %% "discipline" % "0.2.1",
        "org.scalatest" %% "scalatest" % "2.2.3" % "test"
      )
    )
}




