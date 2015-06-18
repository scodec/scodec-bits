import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaKeys._

lazy val commonSettings = Seq(
  scodecModule := "scodec-bits",
  rootPackage := "scodec.bits",
  contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"), Contributor("pchiusano", "Paul Chiusano")),
  scalaVersion := "2.12.0-M1",
  crossScalaVersions := Seq(scalaVersion.value)
)

lazy val root = project.in(file(".")).aggregate(core).settings(commonSettings: _*).settings(
  publishArtifact := false
)

lazy val core = project.in(file("core")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
  settings(scodecPrimaryModule: _*).
  settings(scodecPrimaryModuleJvm: _*).
  settings(
    unmanagedSourceDirectories in Compile += baseDirectory.value / "shared" / "src" / "main" / "scala",
    unmanagedSourceDirectories in Test += baseDirectory.value / "shared" / "src" / "test" / "scala"
  ).
  settings(
    scodecModule := "scodec-bits",
    rootPackage := "scodec.bits",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "org.scalatest" %%% "scalatest" % "2.2.5-M1" % "test",
      "org.scalacheck" %%% "scalacheck" % "1.12.4" % "test")
  ).
  settings(
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
      "scodec.bits.BitVector#Bytes.scodec$bits$BitVector$_setter_$sizeLowerBound_=",
      "scodec.bits.BitVector.toShort",
      "scodec.bits.BitVector.toShort$default$1",
      "scodec.bits.BitVector.toShort$default$2",
      "scodec.bits.BitVector.sliceToShort",
      "scodec.bits.BitVector.sliceToShort$default$3",
      "scodec.bits.BitVector.sliceToShort$default$4",
      "scodec.bits.ByteVector.toShort",
      "scodec.bits.ByteVector.toShort$default$1",
      "scodec.bits.ByteVector.toShort$default$2",
      "scodec.bits.ByteVector.toByte",
      "scodec.bits.ByteVector.toByte$default$1",
      "scodec.bits.BitVector.toByte",
      "scodec.bits.BitVector.toByte$default$1",
      "scodec.bits.BitVector.sliceToByte",
      "scodec.bits.BitVector.sliceToByte$default$3",
      "scodec.bits.BitVector.invertReverseByteOrder",
      "scodec.bits.ByteVector.decrypt",
      "scodec.bits.ByteVector.decrypt$default$3",
      "scodec.bits.ByteVector.cipher$default$4",
      "scodec.bits.ByteVector.encrypt",
      "scodec.bits.ByteVector.cipher",
      "scodec.bits.ByteVector.encrypt$default$3",
      "scodec.bits.BitVector.decrypt",
      "scodec.bits.BitVector.decrypt$default$3",
      "scodec.bits.BitVector.cipher$default$4",
      "scodec.bits.BitVector.encrypt",
      "scodec.bits.BitVector.cipher",
      "scodec.bits.BitVector.encrypt$default$3",
      "scodec.bits.ByteVector.copyToArray",
      "scodec.bits.ByteVector.deflate",
      "scodec.bits.ByteVector.deflate$default$1",
      "scodec.bits.ByteVector.deflate$default$2",
      "scodec.bits.ByteVector.deflate$default$3",
      "scodec.bits.ByteVector.deflate$default$4",
      "scodec.bits.ByteVector.inflate",
      "scodec.bits.ByteVector.inflate$default$1",
      "scodec.bits.ByteVector.inflate$default$2",
      "scodec.bits.BitVector.deflate",
      "scodec.bits.BitVector.deflate$default$1",
      "scodec.bits.BitVector.deflate$default$2",
      "scodec.bits.BitVector.deflate$default$3",
      "scodec.bits.BitVector.deflate$default$4",
      "scodec.bits.BitVector.inflate",
      "scodec.bits.BitVector.inflate$default$1",
      "scodec.bits.BitVector.inflate$default$2",
      "scodec.bits.ByteVector.takeWhile",
      "scodec.bits.ByteVector.dropWhile",
      "scodec.bits.ByteVector.foreachSPartial",
      "scodec.bits.ByteVector.foreachVPartial",
      "scodec.bits.BitVector.decodeString",
      "scodec.bits.BitVector.decodeAscii",
      "scodec.bits.BitVector.decodeUtf8",
      "scodec.bits.ByteVector.decodeString",
      "scodec.bits.ByteVector.decodeAscii",
      "scodec.bits.ByteVector.decodeUtf8"
    ).map { method => ProblemFilters.exclude[MissingMethodProblem](method) },
    binaryIssueFilters ++= Seq(
      // result type changed, but this method is private
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("scodec.bits.BitVector#Append.sizeLowerBound"),
      // param type changed, but this method is private
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("scodec.bits.crc.scodec$bits$crc$$calculate$1")
    )
)
