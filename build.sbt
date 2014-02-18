import sbtrelease._
import ReleaseStateTransformations._
import ReleasePlugin._
import ReleaseKeys._

organization := "org.typelevel"

name := "scodec-bits"

scalaVersion := "2.10.3"

val latestScala211PreRelease = "2.11.0-M8"

scalaBinaryVersion in update := (
  if (scalaVersion.value == "2.11.0-SNAPSHOT") latestScala211PreRelease else scalaBinaryVersion.value
)

crossScalaVersions := Seq("2.10.3", latestScala211PreRelease)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-optimise",
  "-Xcheckinit",
  "-Xlint",
  "-Xverify",
  "-Yclosure-elim",
  "-Yinline",
  "-Ywarn-all")

scalacOptions in (Compile, doc) ++= Seq(
  "-diagrams",
  "-groups",
  "-implicits",
  "-implicits-show-all"
)

licenses += ("Three-clause BSD-style", url("http://github.com/scodec/scodec/blob/master/LICENSE"))

unmanagedResources in Compile <++= baseDirectory map { base => Seq(base / "NOTICE", base / "LICENSE") }

triggeredMessage := (_ => Watched.clearScreen)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "2.1.0-RC2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

osgiSettings

OsgiKeys.exportPackage := Seq("scodec.bits.*;version=${Bundle-Version}")

OsgiKeys.importPackage := Seq(
  """scala.*;version="$<range;[==,=+);$<@>>"""",
  "*"
)

OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

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
)

pomPostProcess := { (node) =>
  import scala.xml._
  import scala.xml.transform._
  def stripIf(f: Node => Boolean) = new RewriteRule {
    override def transform(n: Node) =
      if (f(n)) NodeSeq.Empty else n
  }
  val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
  new RuleTransformer(stripTestScope).transform(node)(0)
}

releaseSettings

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts.copy(action = publishSignedAction),
  releaseTask(GhPagesKeys.pushSite),
  setNextVersion,
  commitNextVersion,
  pushChanges
)

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:scodec/scodec-bits.git"
