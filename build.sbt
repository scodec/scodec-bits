import com.typesafe.tools.mima.core._
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import com.typesafe.sbt.SbtGit.GitKeys.{gitCurrentBranch, gitHeadCommit}

addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", "; compile:scalafmtCheck; test:scalafmtCheck; scalafmtSbtCheck")

crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.11", "2.13.2", "0.26.0-RC1")

scalaVersion in ThisBuild := crossScalaVersions.value.head

githubWorkflowJavaVersions in ThisBuild := Seq("adopt@1.11")
githubWorkflowPublishTargetBranches in ThisBuild := Seq(RefPredicate.Equals(Ref.Branch("main")))
githubWorkflowBuild in ThisBuild := Seq(
  WorkflowStep.Sbt(List("compile")),
  WorkflowStep.Sbt(List("coreJVM/test")),
  WorkflowStep.Sbt(List("coreJS/test")),
  WorkflowStep.Sbt(List("mimaReportBinaryIssues"))
)

lazy val contributors = Seq(
  "mpilquist" -> "Michael Pilquist",
  "pchiusano" -> "Paul Chiusano"
)

lazy val commonSettings = Seq(
  organization := "org.scodec",
  organizationHomepage := Some(new URL("http://scodec.org")),
  licenses += ("Three-clause BSD-style", url(
    "https://github.com/scodec/scodec-bits/blob/main/LICENSE"
  )),
  git.remoteRepo := "git@github.com:scodec/scodec-bits.git",
  scmInfo := Some(
    ScmInfo(url("https://github.com/scodec/scodec-bits"), "git@github.com:scodec/scodec-bits.git")
  ),
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
      case v if v.startsWith("2.11") =>
        Nil
      case v if v.startsWith("0.") =>
        Nil
      case other => sys.error(s"Unsupported scala version: $other")
    }),
  testFrameworks += new TestFramework("munit.Framework"),
  releaseCrossBuild := true,
  releaseProcess := {
    import sbtrelease.ReleaseStateTransformations._
    Seq[ReleaseStep](
      inquireVersions,
      runClean,
      releaseStepCommandAndRemaining("+test"),
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      releaseStepCommandAndRemaining("+publish"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  },
  // Needed b/c coreJS is published twice due to crossScalaVersions override
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  Compile / doc / sources := {
    val old = (Compile / doc / sources).value
    if (isDotty.value)
      Seq()
    else
      old
  }
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
  pomIncludeRepository := { x => false },
  pomExtra := (
    <url>http://github.com/scodec/scodec-bits</url>
    <developers>
      {
      for ((username, name) <- contributors)
        yield <developer>
        <id>{username}</id>
        <name>{name}</name>
        <url>http://github.com/{username}</url>
      </developer>
    }
    </developers>
  ),
  pomPostProcess := { node =>
    import scala.xml._
    import scala.xml.transform._
    def stripIf(f: Node => Boolean) =
      new RewriteRule {
        override def transform(n: Node) =
          if (f(n)) NodeSeq.Empty else n
      }
    val stripTestScope = stripIf(n => n.label == "dependency" && (n \ "scope").text == "test")
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
    libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "0.7.11" % "test",
    libraryDependencies ++= {
      if (isDotty.value) Nil
      else Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided")
    },
    crossScalaVersions := {
      val default = crossScalaVersions.value
      if (crossProjectPlatform.value.identifier != "jvm")
        default.filter(_.startsWith("2."))
      else
        default
    },
    autoAPIMappings := true,
    buildInfoPackage := "scodec.bits",
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion, gitHeadCommit),
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
      _.filterNot(o => o == "-Ywarn-unused" || o == "-Xfatal-warnings")
    },
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
    mimaPreviousArtifacts := {
      if (isDotty.value) Set.empty
      else List("1.1.17").map { pv =>
        organization.value % (normalizedName.value + "_" + scalaBinaryVersion.value) % pv
      }.toSet
    },
    mimaBinaryIssueFilters ++= Seq()
  )

lazy val coreJVM = core.jvm.settings(
  libraryDependencies ++= Seq(
    "com.google.guava" % "guava" % "23.6.1-jre" % "test"
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
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
)

lazy val benchmark: Project = project
  .in(file("benchmark"))
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    mimaPreviousArtifacts := Set.empty
  )
