import com.typesafe.tools.mima.core._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import ReleaseTransformations._

lazy val commonSettings = Seq(
  scodecModule := "scodec-bits",
  rootPackage := "scodec.bits",
  scmInfo := Some(ScmInfo(url("https://github.com/scodec/scodec-bits"), "git@github.com:scodec/scodec-bits.git")),
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
  crossScalaVersions += "2.10.7",
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
    releaseStepCommandAndRemaining("+publish"),
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
    ),
    unmanagedSourceDirectories in Compile += {
      val dir = CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 => "scala_2.13"
        case _ => "scala_pre_2.13"
      }
      baseDirectory.value / "../shared/src/main" /  dir
    }
  ).
  platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % "1.14.0" % "test"
    ),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.0-SNAP10" % "test"
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
    docSourcePath := new File(baseDirectory.value, "../.."),
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "23.0" % "test",
      "com.google.code.findbugs" % "jsr305" % "3.0.2" % "test" // required for guava
    ),
    OsgiKeys.privatePackage := Nil,
    OsgiKeys.exportPackage := Seq("scodec.bits.*;version=${Bundle-Version}"),
    OsgiKeys.importPackage := Seq(
      """scala.*;version="$<range;[==,=+)>"""",
      "*"
    ),
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("scodec.bits.BitVector.reduceBalanced")
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
