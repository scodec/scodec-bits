scodec-bits
===========

Provides persistent datatypes for working with bits and bytes.

Design Constraints
------------------
 - No dependencies on other libraries in order to be applicable to as many projects as possible.

Introduction
------------

There are two fundamental data structures provided:
 - [`BitVector`](core/src/main/scala/scodec/bits/BitVector.scala)
 - [`ByteVector`](core/src/main/scala/scodec/bits/ByteVector.scala)

Both datatypes provide collection like functionality, although neither directly implement the Scala
collection library traits. Both are implemented as binary trees where each node is either a leaf
node containing bits/bytes or is an inner node containing an operation on other trees (e.g., append).
As a result, many operations are implemented in logarithmic time with respect to the depth of the tree.

Both strict and lazy constructors are provided. Of particular interest are the methods that work
with Java NIO sources like `java.nio.channels.ReadableByteChannel` and `java.nio.channels.FileChannel`,
which allow efficient, lazy access and manipulation of bits and bytes from a variety of sources.

Hexadecimal and binary string literals are supported via the `hex` and `bin` string interpolators.
For example:

    val x: ByteVector = hex"deadbeef"
    val y: BitVector = bin"00101101010010101"

Base conversions to and from strings are supported. Binary, hexadecimal, and base 64 conversions are
supported. Each have a default alphabet but support alternative alphabets as well as user defined
alphabets.

For more information, see the [API documentation](http://scodec.github.io/scodec-bits/latest/api/#scodec.package).

For a combinator based approach to encoding values to/from binary, see the companion project [scodec](https://github.com/scodec/scodec).

Getting Binaries
----------------

This library works with Scala 2.10 and 2.11.

### Releases

The latest released version is 1.0.4.

 - [ScalaDoc](http://docs.typelevel.org/api/scodec/bits/stable/1.0.4/)

For SBT users:

    libraryDependencies += "org.typelevel" %% "scodec-bits" % "1.0.4"


For Maven users:

    <dependencies>
      <dependency>
        <groupId>org.typelevel</groupId>
        <artifactId>scodec-bits_2.11</artifactId>
        <version>1.0.4</version>
      </dependency>
    </dependencies>


### Snapshots

Snapshot builds of the master branch are available on Sonatype's OSS hosting at https://oss.sonatype.org/content/repositories/snapshots/.

For SBT users:

    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

    libraryDependencies += "org.typelevel" %% "scodec-bits" % "1.1.0-SNAPSHOT"


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
        <groupId>org.typelevel</groupId>
        <artifactId>scodec-bits_2.10</artifactId>
        <version>1.1.0-SNAPSHOT</version>
      </dependency>
    </dependencies>

Building
--------

This project uses sbt. To build, run `sbt publish-local`.
