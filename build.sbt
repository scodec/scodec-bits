import com.typesafe.tools.mima.core._
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import com.typesafe.sbt.SbtGit.GitKeys.{gitCurrentBranch, gitHeadCommit}

addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", "; compile:scalafmtCheck; test:scalafmtCheck; scalafmtSbtCheck")

lazy val contributors = Seq(
  "mpilquist" -> "Michael Pilquist",
  "pchiusano" -> "Paul Chiusano"
)

lazy val commonSettings = Seq(
  organization := "org.scodec",
  organizationHomepage := Some(new URL("http://scodec.org")),
  licenses += ("Three-clause BSD-style", url(
    "https://github.com/scodec/scodec-bits/blob/master/LICENSE"
  )),
  git.remoteRepo := "git@github.com:scodec/scodec-bits.git",
  scmInfo := Some(
    ScmInfo(url("https://github.com/scodec/scodec-bits"), "git@github.com:scodec/scodec-bits.git")
  ),
  Compile / unmanagedSourceDirectories ++= {
    if (isDotty.value)
      List(CrossType.Pure, CrossType.Full).flatMap(
        _.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + "-3"))
      )
    else Nil
  },
  unmanagedResources in Compile ++= {
    val base = baseDirectory.value
    (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get
  },
  scalacOptions ++= Seq(
    "-encoding",
    "UTF-8",
    "-deprecation",
    "-feature",
    "-unchecked"
  ) ++
    (scalaBinaryVersion.value match {
      case v if v.startsWith("2.13") =>
        List("-Xlint", "-Ywarn-unused")
      case v if v.startsWith("2.12") =>
        Nil
      case v if v.startsWith("0.") =>
        Nil
      case other => sys.error(s"Unsupported scala version: $other")
    }),
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
  releaseCrossBuild := true
) ++ publishingSettings

lazy val publishingSettings = Seq(
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots".at(nexus + "content/repositories/snapshots"))
    else
      Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
  },
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { x =>
    false
  },
  pomExtra := (
    <url>http://github.com/scodec/scodec-bits</url>
    <developers>
      {for ((username, name) <- contributors) yield <developer>
        <id>{username}</id>
        <name>{name}</name>
        <url>http://github.com/{username}</url>
      </developer>}
    </developers>
  ),
  pomPostProcess := { (node) =>
    import scala.xml._
    import scala.xml.transform._
    def stripIf(f: Node => Boolean) = new RewriteRule {
      override def transform(n: Node) =
        if (f(n)) NodeSeq.Empty else n
    }
    val stripTestScope = stripIf { n =>
      n.label == "dependency" && (n \ "scope").text == "test"
    }
    new RuleTransformer(stripTestScope).transform(node)(0)
  }
)

lazy val root = project
  .in(file("."))
  .aggregate(coreJVM, coreJS, benchmark)
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    mimaPreviousArtifacts := Set.empty
  )

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("core"))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "scodec-bits",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.1.1" % "test",
      ("org.scalatestplus" %%% "scalacheck-1-14" % "3.1.1.1" % "test")
        .intransitive()
        .withDottyCompat(scalaVersion.value),
      ("org.scalacheck" %%% "scalacheck" % "1.14.3" % "test").withDottyCompat(scalaVersion.value)
    ),
    libraryDependencies ++= {
      if (isDotty.value) Nil
      else Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided")
    },
    autoAPIMappings := true,
    buildInfoPackage := "scodec.bits",
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion, gitHeadCommit),
    publishArtifact in (Compile, packageDoc) := !isDotty.value,
    scalacOptions in (Compile, doc) := {
      val tagOrBranch = {
        if (version.value.endsWith("SNAPSHOT")) gitCurrentBranch.value
        else ("v" + version.value)
      }
      if (isDotty.value) Nil
      else
        Seq(
          "-groups",
          "-implicits",
          "-implicits-show-all",
          "-sourcepath",
          new File(baseDirectory.value, "../..").getCanonicalPath,
          "-doc-source-url",
          "https://github.com/scodec/scodec-bits/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
        )
    },
    scalacOptions in (Compile, console) ~= {
      _.filterNot { o =>
        o == "-Ywarn-unused" || o == "-Xfatal-warnings"
      }
    },
    publishArtifact in (Compile, packageDoc) := !isDotty.value,
    publishArtifact in packageDoc := !isDotty.value,
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
    mimaPreviousArtifacts := {
      List("1.1.12").map { pv =>
        organization.value % (normalizedName.value + "_" + scalaBinaryVersion.value) % pv
      }.toSet
    },
    mimaBinaryIssueFilters ++= Seq(
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
      ProblemFilters.exclude[MissingClassProblem]("scodec.bits.ScalaVersionSpecific")
    )
  )

lazy val coreJVM = core.jvm.settings(
  libraryDependencies ++= Seq(
    "com.google.guava" % "guava" % "23.0" % "test"
  ),
  OsgiKeys.privatePackage := Nil,
  OsgiKeys.exportPackage := Seq("scodec.bits.*;version=${Bundle-Version}"),
  OsgiKeys.importPackage := Seq(
    """scala.*;version="$<range;[==,=+)>"""",
    "*"
  ),
  OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
)

lazy val coreJS = core.js

lazy val benchmark: Project = project
  .in(file("benchmark"))
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    mimaPreviousArtifacts := Set.empty
  )
