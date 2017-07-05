import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaKeys._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val commonSettings = Seq(
  scodecModule := "scodec-bits",
  rootPackage := "scodec.bits",
  crossScalaVersions += "2.13.0-M1",
  contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"), Contributor("pchiusano", "Paul Chiusano"))
)

lazy val root = project.in(file(".")).aggregate(coreJVM, coreJS, benchmark).settings(commonSettings: _*).settings(
  publishArtifact := false
)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("core")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
  settings(scodecPrimaryModule: _*).
  jvmSettings(scodecPrimaryModuleJvm: _*).
  settings(
    scodecModule := "scodec-bits",
    rootPackage := "scodec.bits",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
    )
  ).
  platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.3" % "test",
      "org.scalacheck" %%% "scalacheck" % "1.13.5" % "test")
  ).
  jsSettings(commonJsSettings: _*).
  jvmSettings(
    previousArtifacts := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 => Set.empty
        case _ => previousArtifacts.value
      }
    },
    previousArtifact := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 => None
        case _ => previousArtifact.value
      }
    },
    docSourcePath := new File(baseDirectory.value, "../.."),
    libraryDependencies ++= Seq(
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
    ).map { method => ProblemFilters.exclude[MissingMethodProblem](method) },
    binaryIssueFilters ++= Seq(
    )
)

val Scala211 = "2.11.11"

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native.settings(
  scalaVersion := Scala211
)

lazy val nativeTest = project.dependsOn(coreNative).settings(
  commonSettings,
  publishArtifact := false,
  publishLocal := {},
  PgpKeys.publishSigned := {},
  scalaVersion := Scala211
).enablePlugins(ScalaNativePlugin)

lazy val benchmark: Project = project.in(file("benchmark")).dependsOn(coreJVM).enablePlugins(JmhPlugin).
  settings(commonSettings: _*).
  settings(
    publishArtifact := false
  )
