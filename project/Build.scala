import sbt._
import Keys._
import com.typesafe.sbt.osgi.SbtOsgi._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaKeys._
import pl.project13.scala.sbt.SbtJmh._
import scodec.build.ScodecBuildSettings.autoImport._

object ScodecBuild extends Build {

  lazy val commonSettings = Seq(
    scodecModule := "scodec-bits",
    rootPackage := "scodec.bits",
    contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"), Contributor("pchiusano", "Paul Chiusano"))
  )

  lazy val root: Project = project.in(file(".")).aggregate(core, benchmark).settings(commonSettings: _*).settings(
    publishArtifact := false
  )

  lazy val core: Project = project.in(file("core")).
    settings(commonSettings: _*).
    settings(scodecPrimaryModule: _*).
    settings(
      scodecModule := "scodec-bits",
      rootPackage := "scodec.bits",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
        "org.scalatest" %% "scalatest" % "2.1.3" % "test",
        "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
        "com.google.guava" % "guava" % "16.0.1" % "test",
        "com.google.code.findbugs" % "jsr305" % "2.0.3" % "test" // required for guava
      ),
      OsgiKeys.privatePackage := Nil,
      OsgiKeys.exportPackage := Seq("scodec.bits.*;version=${Bundle-Version}"),
      OsgiKeys.importPackage := Seq(
        """scala.*;version="$<range;[==,=+)>"""",
        "*"
      ),
      binaryIssueFilters ++= Seq(
      ).map { method => ProblemFilters.exclude[MissingMethodProblem](method) }
  )

  lazy val benchmark: Project = project.in(file("benchmark")).dependsOn(core).settings(jmhSettings: _*).
    settings(commonSettings: _*).
    settings(
      publishArtifact := false,
      libraryDependencies ++=
        Seq("com.typesafe.akka" %% "akka-actor" % "2.3.5")
    )
}
