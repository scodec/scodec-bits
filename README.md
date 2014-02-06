scodec-bits
===========

Provides immutable datatypes for working with bits and bytes.

Design Constraints
------------------
 - No dependencies on other libraries in order to be applicable to as many projects as possible.

Introduction
------------

There are two fundamental data structures provided:
 - [`BitVector`](src/main/scala/scodec/bits/BitVector.scala)
 - [`ByteVector`](src/main/scala/scodec/bits/ByteVector.scala)

TODO - examples, etc.

For more information, see the [package documentation](src/main/scala/scodec/bits/package.scala)

For a combinator based approach to encoding values to/from binary, see the companion project [scodec](https://github.com/scodec/scodec).

Getting Binaries
----------------

This library works with Scala 2.10.* and 2.11.0-M8.

### Releases

The latest released version is 1.0.0-SNAP1, which is a stable snapshot of 1.0.0.

For SBT users:

    libraryDependencies += "com.github.scodec" %% "scodec-bits" % "1.0.0-SNAP1"


For Maven users:

    <dependencies>
      <dependency>
        <groupId>com.github.scodec</groupId>
        <artifactId>scodec-bits_2.10</artifactId>
        <version>1.0.0-SNAP1</version>
      </dependency>
    </dependencies>


### Snapshots

Snapshot builds of the master branch are available on Sonatype's OSS hosting at https://oss.sonatype.org/content/repositories/snapshots/.

For SBT users:

    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

    libraryDependencies += "com.github.scodec" %% "scodec-bits" % "1.0.0-SNAPSHOT"


For Maven users:

    <repositories>
      <repository>
        <id>sonatype-oss-snapshots</id>
        <name>Sonatype OSS Snapshots</name>
        <url>https://oss.sonatype.org/content/repositories/snapshots/</url>
      </repository>
    </repositories>

    <dependencies>
      <dependency>
        <groupId>com.github.scodec</groupId>
        <artifactId>scodec-bits_2.10</artifactId>
        <version>1.0.0-SNAPSHOT</version>
      </dependency>
    </dependencies>

Building
--------

This project uses sbt. To build, run `sbt publish-local`.
