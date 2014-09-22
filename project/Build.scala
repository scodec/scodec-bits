import sbt._
import Keys._
import sbtrelease._
import ReleaseStateTransformations._
import ReleasePlugin._
import ReleaseKeys._
import Utilities._
import com.typesafe.sbt.osgi.SbtOsgi._
import com.typesafe.sbt.SbtGhPages._
import com.typesafe.sbt.SbtGit.git
import com.typesafe.sbt.SbtPgp.PgpKeys._
import com.typesafe.sbt.SbtSite._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys._
import pl.project13.scala.sbt.SbtJmh._

object ScodecBuild extends Build {

  lazy val commonSettings: Seq[Setting[_]] = Seq(
    organization := "org.typelevel",
    scalaVersion := "2.10.4",
    crossScalaVersions := Seq("2.10.4", "2.11.0"),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-optimise",
      "-Xcheckinit",
      "-Xlint",
      "-Xverify",
      "-Yclosure-elim",
      "-Yinline"),
    scalacOptions in (Compile, doc) ++= {
      val tagOrBranch = if (version.value endsWith "SNAPSHOT") "master" else ("v" + version.value)
      Seq(
        "-diagrams",
        "-groups",
        "-implicits",
        "-implicits-show-all",
        "-sourcepath", baseDirectory.value.getAbsolutePath,
        "-doc-source-url", "https:///github.com/scodec/scodec-bits/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
      )
    },
    testOptions in Test += Tests.Argument("-oD"),
    licenses += ("Three-clause BSD-style", url("http://github.com/scodec/scodec-bits/blob/master/LICENSE")),
    triggeredMessage := (_ => Watched.clearScreen),
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>http://github.com/scodec/scodec-bits</url>
      <scm>
        <url>git@github.com:scodec/scodec-bits.git</url>
        <connection>scm:git:git@github.com:scodec/scodec-bits.git</connection>
      </scm>
      <developers>
        <developer>
          <id>mpilquist</id>
          <name>Michael Pilquist</name>
          <url>http://github.com/mpilquist</url>
        </developer>
        <developer>
          <id>pchiusano</id>
          <name>Paul Chiusano</name>
          <url>http://github.com/pchiusano</url>
        </developer>
      </developers>
    ),
    pomPostProcess := { (node) =>
      import scala.xml._
      import scala.xml.transform._
      def stripIf(f: Node => Boolean) = new RewriteRule {
        override def transform(n: Node) =
          if (f(n)) NodeSeq.Empty else n
      }
      val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
      new RuleTransformer(stripTestScope).transform(node)(0)
    },
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts.copy(action = publishSignedAction),
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  ) ++ releaseSettings

  lazy val root: Project = project.in(file(".")).settings(commonSettings: _*).aggregate(core, benchmark).settings(
    publishArtifact := false
  )

  lazy val core: Project = project.in(file("core")).
    settings((commonSettings ++ site.settings ++ site.includeScaladoc() ++ ghpages.settings ++ mimaDefaultSettings ++ osgiSettings): _*).
    settings(
      name := "scodec-bits",
      autoAPIMappings := true,
      apiURL := Some(url(s"http://docs.typelevel.org/api/scodec/bits/stable/${version.value}/")),
      unmanagedResources in Compile <++= baseDirectory map { base => Seq(base / "NOTICE", base / "LICENSE") },
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
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
      OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package"),
      git.remoteRepo := "git@github.com:scodec/scodec-bits.git",
      previousArtifact := previousVersion(version.value) map { pv =>
        organization.value % (normalizedName.value + "_" + scalaBinaryVersion.value) % pv
      },
      binaryIssueFilters ++= Seq(
        "scodec.bits.ByteVector.buffer",
        "scodec.bits.ByteVector.bufferBy",
        "scodec.bits.ByteVector.unbuffer",
        "scodec.bits.ByteVector.getImpl",
        "scodec.bits.BitVector#Bytes.depthExceeds",
        "scodec.bits.BitVector#Append.depthExceeds",
        "scodec.bits.BitVector#Suspend.depthExceeds",
        "scodec.bits.BitVector#Drop.depthExceeds",
        "scodec.bits.BitVector.depthExceeds",
        "scodec.bits.BitVector.depth",
        "scodec.bits.BitVector.unchunk",
        "scodec.bits.BitVector.align",
        "scodec.bits.BitVector.scodec$bits$BitVector$$reduceBalanced",
        "scodec.bits.BitVector#Drop.sizeIsAtMost",
        "scodec.bits.BitVector#Drop.sizeIsAtLeast",
        "scodec.bits.BitVector#Drop.sizeUpperBound",
        "scodec.bits.BitVector#Drop.sizeLowerBound",
        "scodec.bits.BitVector#Drop.scodec$bits$BitVector$_setter_$sizeUpperBound_=",
        "scodec.bits.BitVector#Drop.scodec$bits$BitVector$_setter_$sizeLowerBound_=",
        "scodec.bits.BitVector#Append.sizeIsAtMost",
        "scodec.bits.BitVector#Append.sizeIsAtLeast",
        "scodec.bits.BitVector#Append.sizeUpperBound",
        "scodec.bits.BitVector#Append.sizeLowerBound",
        "scodec.bits.BitVector#Append.scodec$bits$BitVector$_setter_$sizeUpperBound_=",
        "scodec.bits.BitVector#Append.scodec$bits$BitVector$_setter_$sizeLowerBound_=",
        "scodec.bits.BitVector#Chunks.sizeIsAtMost",
        "scodec.bits.BitVector#Chunks.sizeIsAtLeast",
        "scodec.bits.BitVector#Chunks.sizeUpperBound",
        "scodec.bits.BitVector#Chunks.sizeLowerBound",
        "scodec.bits.BitVector#Chunks.scodec$bits$BitVector$_setter_$sizeUpperBound_=",
        "scodec.bits.BitVector#Chunks.scodec$bits$BitVector$_setter_$sizeLowerBound_=",
        "scodec.bits.BitVector.sizeIsAtMost",
        "scodec.bits.BitVector.sizeIsAtLeast",
        "scodec.bits.BitVector.sizeUpperBound",
        "scodec.bits.BitVector.take",
        "scodec.bits.BitVector.sizeLowerBound",
        "scodec.bits.BitVector.drop",
        "scodec.bits.BitVector.scodec$bits$BitVector$_setter_$sizeUpperBound_=",
        "scodec.bits.BitVector.sizeLessThan",
        "scodec.bits.BitVector.scodec$bits$BitVector$_setter_$sizeLowerBound_=",
        "scodec.bits.BitVector.sliceToInt",
        "scodec.bits.BitVector.acquireThen",
        "scodec.bits.BitVector.sliceToLong",
        "scodec.bits.BitVector.sliceToLong$default$4",
        "scodec.bits.BitVector.sliceToInt$default$4",
        "scodec.bits.BitVector.sliceToLong$default$3",
        "scodec.bits.BitVector.consumeThen",
        "scodec.bits.BitVector.sliceToInt$default$3",
        "scodec.bits.BitVector.getByte",
        "scodec.bits.BitVector#Suspend.sizeIsAtMost",
        "scodec.bits.BitVector#Suspend.sizeIsAtLeast",
        "scodec.bits.BitVector#Suspend.sizeUpperBound",
        "scodec.bits.BitVector#Suspend.sizeLowerBound",
        "scodec.bits.BitVector#Suspend.scodec$bits$BitVector$_setter_$sizeUpperBound_=",
        "scodec.bits.BitVector#Suspend.scodec$bits$BitVector$_setter_$sizeLowerBound_=",
        "scodec.bits.BitVector#Bytes.sizeIsAtMost",
        "scodec.bits.BitVector#Bytes.sizeIsAtLeast",
        "scodec.bits.BitVector#Bytes.sizeUpperBound",
        "scodec.bits.BitVector#Bytes.sizeLowerBound",
        "scodec.bits.BitVector#Bytes.scodec$bits$BitVector$_setter_$sizeUpperBound_=",
        "scodec.bits.BitVector#Bytes.scodec$bits$BitVector$_setter_$sizeLowerBound_="
      ).map { method => ProblemFilters.exclude[MissingMethodProblem](method) },
      binaryIssueFilters +=
        // result type changed, but this method was private
        ProblemFilters.exclude[IncompatibleResultTypeProblem]("scodec.bits.BitVector#Append.sizeLowerBound")
  )

  lazy val benchmark: Project = project.in(file("benchmark")).settings(commonSettings: _*).dependsOn(core).settings(jmhSettings: _*).settings(
    publishArtifact := false,
    libraryDependencies ++=
      Seq("com.typesafe.akka" %% "akka-actor" % "2.3.5")
  )


  lazy val publishSignedAction = { st: State =>
    val extracted = st.extract
    val ref = extracted.get(thisProjectRef)
    extracted.runAggregated(publishSigned in Global in ref, st)
  }

  private def previousVersion(currentVersion: String): Option[String] = {
    val Version = """(\d+)\.(\d+)\.(\d+).*""".r
    val Version(x, y, z) = currentVersion
    if (z == "0") None
    else Some(s"$x.$y.${z.toInt - 1}")
  }
}
