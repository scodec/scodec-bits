import com.typesafe.tools.mima.core._

addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", "; compile:scalafmtCheck; test:scalafmtCheck; scalafmtSbtCheck")

ThisBuild / tlBaseVersion := "1.1"

ThisBuild / organization := "org.scodec"
ThisBuild / organizationName := "Scodec"

ThisBuild / startYear := Some(2013)

ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.16", "2.13.8", "3.1.3")

ThisBuild / tlVersionIntroduced := Map(
  "3" -> "1.1.27",
  "2.13" -> "1.1.12",
  "2.12" -> "1.1.2",
  "2.11" -> "1.1.99" // Ignore 2.11 in mima
)

ThisBuild / tlMimaPreviousVersions ~= (_.filterNot(_ == "1.1.32"))

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("8"))

ThisBuild / tlFatalWarningsInCi := false

ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/scodec/scodec-bits"), "git@github.com:scodec/scodec-bits.git")
)

ThisBuild / licenses := List(
  ("BSD-3-Clause", url("https://github.com/scodec/scodec-bits/blob/main/LICENSE"))
)

ThisBuild / developers ++= List(
  tlGitHubDev("mpilquist", "Michael Pilquist"),
  tlGitHubDev("pchiusano", "Paul Chiusano")
)

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
  ProblemFilters.exclude[IncompatibleMethTypeProblem]("scodec.bits.BitVector.reduceBalanced"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.BuildInfo"),
  ProblemFilters.exclude[MissingClassProblem]("scodec.bits.BuildInfo$"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.crc.vectorTable"),
  ProblemFilters.exclude[IncompatibleMethTypeProblem](
    "scodec.bits.ByteVector#ByteVectorInputStream#CustomAtomicInteger.getAndUpdate_"
  ),
  ProblemFilters.exclude[IncompatibleMethTypeProblem]("scodec.bits.HexDumpFormat.render"),
  ProblemFilters.exclude[IncompatibleMethTypeProblem]("scodec.bits.HexDumpFormat.print"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.HexDumpFormat.this")
)

lazy val root = tlCrossRootProject.aggregate(core, benchmark)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("core"))
  .settings(
    name := "scodec-bits",
    libraryDependencies ++= {
      if (tlIsScala3.value) Nil
      else Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided")
    },
    Compile / unmanagedResources ++= {
      val base = baseDirectory.value
      (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get
    },
    scalacOptions := scalacOptions.value.filterNot(_ == "-source:3.0-migration"),
    libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "1.0.0-M4" % "test"
  )

lazy val coreJVM = core.jvm
  .settings(
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "31.1-jre" % "test"
    )
  )

lazy val coreJS = core.js.settings(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  // Override JS versions, as older stuff built for SJS 0.6
  tlVersionIntroduced := tlVersionIntroduced.value ++ Map(
    "2.13" -> "1.1.14",
    "2.12" -> "1.1.14"
  ),
  mimaBinaryIssueFilters ++= Seq(
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.deflate"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.deflate$default$1"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.deflate$default$2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.deflate$default$3"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.deflate$default$4"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.inflate"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.inflate$default$1"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.digest"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.digest"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.encrypt"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.encrypt$default$3"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.decrypt"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.decrypt$default$3"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.deflate"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.deflate$default$1"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.deflate$default$2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.deflate$default$3"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.deflate$default$4"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.inflate"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.inflate$default$1"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.inflate$default$2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.digest"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.encrypt"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.encrypt$default$3"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.decrypt"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.decrypt$default$3"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.cipher"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.BitVector.cipher$default$4"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.cipher"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.bits.ByteVector.cipher$default$4")
  )
)

lazy val coreNative = core.native.settings(
  tlVersionIntroduced ++= List("2.12", "2.13", "3").map(_ -> "1.1.32").toMap
)

lazy val benchmark: Project = project
  .in(file("benchmark"))
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin, NoPublishPlugin)
