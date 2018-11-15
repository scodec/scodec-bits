import com.typesafe.tools.mima.core._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import ReleaseTransformations._

lazy val commonSettings = Seq(
  scodecModule := "scodec-bits",
  rootPackage := "scodec.bits",
  contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"), Contributor("pchiusano", "Paul Chiusano")),
  scalacOptions --= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        Seq("-Yno-adapted-args", "-Ywarn-unused-import")
      case _ =>
        Nil
    }
  },
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        Seq("-Ywarn-unused:imports")
      case _ =>
        Nil
    }
  },
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  releaseCrossBuild := false,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    releaseStepCommandAndRemaining("+test"),
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    releaseStepCommandAndRemaining("+publishSigned"),
    ReleaseStep(
      check = releaseStepTaskAggregated(makeSite in thisProjectRef.value),
      action = releaseStepTaskAggregated(ghpagesPushSite in thisProjectRef.value)
    ),
    setNextVersion,
    commitNextVersion,
    pushChanges
  )
)

lazy val root = project.in(file(".")).aggregate(coreJVM, coreJS, coreNative, benchmark).settings(commonSettings: _*).settings(
  publishArtifact := false
)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("core")).
  enablePlugins(BuildInfoPlugin).
  enablePlugins(ScodecPrimaryModuleSettings).
  settings(commonSettings: _*).
  settings(
    scodecModule := "scodec-bits",
    name := scodecModule.value,
    rootPackage := "scodec.bits",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
    )
  ).
  platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % "1.14.0" % "test"
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 =>
          Seq(
            "org.scalatest" %%% "scalatest" % "3.0.6-SNAP5" % "test"
          )
        case _ =>
          Seq(
            "org.scalatest" %%% "scalatest" % "3.0.5" % "test"
          )
      }
    }
  ).
  jsSettings(commonJsSettings: _*).
  nativeSettings(
    crossScalaVersions := Seq(Scala211),
    // Don't compile shared/src/test
    unmanagedSourceDirectories.in(Test) := Seq(sourceDirectory.in(Test).value),
    test := Def.taskDyn {
      if (scalaVersion.value == Scala211) run.in(Test).toTask("")
      else Def.task(())
    }.value
  ).
  jvmSettings(
    mimaPreviousArtifacts := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 => Set.empty
        case _ => mimaPreviousArtifacts.value
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
    mimaBinaryIssueFilters ++= Seq(
    ).map { method => ProblemFilters.exclude[MissingMethodProblem](method) },
    mimaBinaryIssueFilters ++= Seq(
    )
)

val Scala211 = "2.11.12"

lazy val coreJVM = core.jvm.enablePlugins(ScodecPrimaryModuleJVMSettings)
lazy val coreJS = core.js
lazy val coreNative = core.native.settings(
  scalaVersion := Scala211,
  crossScalaVersions := Seq(scalaVersion.value)
)

lazy val benchmark: Project = project.in(file("benchmark")).dependsOn(coreJVM).enablePlugins(JmhPlugin).
  settings(commonSettings: _*).
  settings(
    publishArtifact := false
  )
