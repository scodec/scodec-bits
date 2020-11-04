import com.typesafe.tools.mima.core._
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import com.typesafe.sbt.SbtGit.GitKeys.{gitCurrentBranch, gitHeadCommit}

addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", "; compile:scalafmtCheck; test:scalafmtCheck; scalafmtSbtCheck")

ThisBuild / baseVersion := "1.1"

ThisBuild / organization := "org.scodec"
ThisBuild / organizationName := "Scodec"

ThisBuild / homepage := Some(url("https://github.com/scodec/scodec-bits"))
ThisBuild / startYear := Some(2013)

ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.11", "2.13.3", "0.27.0-RC1", "3.0.0-M1")

ThisBuild / strictSemVer := false

ThisBuild / versionIntroduced := Map(
  "3.0.0-M1" -> "1.1.99",
  "0.27.0-RC1" -> "1.1.99",
  "2.13" -> "1.1.12",
  "2.12" -> "1.1.2",
  "2.11" -> "1.1.99" // Ignore 2.11 in mima
)

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)
ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("compile")),
  WorkflowStep.Sbt(List("coreJVM/test")),
  WorkflowStep.Sbt(List("coreJS/test")),
  WorkflowStep.Sbt(List("+mimaReportBinaryIssues"))
)

ThisBuild / githubWorkflowEnv ++= Map(
  "SONATYPE_USERNAME" -> s"$${{ secrets.SONATYPE_USERNAME }}",
  "SONATYPE_PASSWORD" -> s"$${{ secrets.SONATYPE_PASSWORD }}",
  "PGP_SECRET" -> s"$${{ secrets.PGP_SECRET }}"
)

ThisBuild / githubWorkflowTargetTags += "v*"

ThisBuild / githubWorkflowPublishPreamble +=
  WorkflowStep.Run(
    List("echo $PGP_SECRET | base64 -d | gpg --import"),
    name = Some("Import signing key")
  )

ThisBuild / githubWorkflowPublish := Seq(WorkflowStep.Sbt(List("release")))

ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/scodec/scodec-bits"), "git@github.com:scodec/scodec-bits.git")
)

ThisBuild / licenses := List(
  ("BSD-3-Clause", url("https://github.com/scodec/scodec-bits/blob/main/LICENSE"))
)

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

ThisBuild / publishGithubUser := "mpilquist"
ThisBuild / publishFullName := "Michael Pilquist"
ThisBuild / developers ++= List(
  "mpilquist" -> "Michael Pilquist",
  "pchiusano" -> "Paul Chiusano"
).map { case (username, fullName) =>
  Developer(username, fullName, s"@$username", url(s"https://github.com/$username"))
}

ThisBuild / fatalWarningsInCI := false

ThisBuild / mimaBinaryIssueFilters ++= Seq(
  ProblemFilters.exclude[IncompatibleResultTypeProblem]("scodec.bits.ByteVector.grouped"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.ByteVector$GroupedOp"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.ByteVector$GroupedOp$"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.GroupedOp"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector$GroupedOp"),
  ProblemFilters.exclude[IncompatibleResultTypeProblem]("scodec.bits.BitVector.grouped"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.BitVector$GroupedOp"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.BitVector$GroupedOp$"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.GroupedOp"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector$GroupedOp"),
  ProblemFilters.exclude[IncompatibleTemplateDefProblem]("scodec.bits.ScalaVersionSpecific"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.ScalaVersionSpecific$"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.package.EitherOps"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.package$EitherOps"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.package$EitherOps$"),
  ProblemFilters.exclude[IncompatibleMethTypeProblem](
    "scodec.bits.LiteralSyntaxMacros.hexStringInterpolator"
  ),
  ProblemFilters.exclude[IncompatibleMethTypeProblem](
    "scodec.bits.LiteralSyntaxMacros.binStringInterpolator"
  ),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.LiteralSyntaxMacros$blackbox$"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.LiteralSyntaxMacros$blackbox$"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.ScalaVersionSpecific"),
  ProblemFilters.exclude[IncompatibleMethTypeProblem]("scodec.bits.BitVector.reduceBalanced")
)

lazy val commonSettings = Seq(
  Compile / unmanagedSourceDirectories ++= {
    val major = if (isDotty.value) "-3" else "-2"
    List(CrossType.Pure, CrossType.Full).flatMap(
      _.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + major))
    )
  },
  unmanagedResources in Compile ++= {
    val base = baseDirectory.value
    (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get
  },
  scalacOptions := scalacOptions.value.filterNot(_ == "-source:3.0-migration")
)

lazy val root = project
  .in(file("."))
  .aggregate(coreJVM, coreJS, benchmark)
  .settings(commonSettings: _*)
  .settings(noPublishSettings)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("core"))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "scodec-bits",
    libraryDependencies ++= {
      if (isDotty.value) Nil
      else Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided")
    },
    buildInfoPackage := "scodec.bits",
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion, gitHeadCommit)
  )
  .settings(dottyLibrarySettings)
  .settings(dottyJsSettings(ThisBuild / crossScalaVersions))
  .settings(
    libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "0.7.16" % "test"
  )

lazy val coreJVM = core.jvm.enablePlugins(SbtOsgi).settings(osgiSettings).settings(
  libraryDependencies ++= Seq(
    "com.google.guava" % "guava" % "30.0-jre" % "test"
  ),
  OsgiKeys.privatePackage := Nil,
  OsgiKeys.exportPackage := Seq("scodec.bits.*;version=${Bundle-Version}"),
  OsgiKeys.importPackage := Seq(
    """scala.*;version="$<range;[==,=+)>"""",
    "*"
  ),
  OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
)

lazy val coreJS = core.js.settings(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  crossScalaVersions := crossScalaVersions.value.filter(_.startsWith("2."))
)

lazy val benchmark: Project = project
  .in(file("benchmark"))
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings: _*)
  .settings(noPublishSettings)
