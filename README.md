[![Discord](https://img.shields.io/discord/632277896739946517.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/wKn3cpfRVz)
[![Maven Central](https://img.shields.io/maven-central/v/org.scodec/scodec-bits_2.13)](https://maven-badges.herokuapp.com/maven-central/org.scodec/scodec-bits_2.13)

scodec-bits
===========

Provides persistent datatypes for working with bits and bytes.

Design Constraints
------------------
 - No dependencies on other libraries in order to be applicable to as many projects as possible.

Administrative
--------------

This project is licensed under a [3-clause BSD license](LICENSE).

The [scodec channel on Typelevel Discord](https://discord.gg/wKn3cpfRVz) is a good place to go for help.

Introduction
------------

There are two fundamental data structures provided:
 - [`BitVector`](core/shared/src/main/scala/scodec/bits/BitVector.scala)
 - [`ByteVector`](core/shared/src/main/scala/scodec/bits/ByteVector.scala)

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

For more information, see the [API documentation](http://scodec.org/api/).

For a combinator based approach to encoding values to/from binary, see the companion project [scodec](https://github.com/scodec/scodec).

Getting Binaries
----------------

See the [releases page on the website](http://scodec.org/releases/).

Building
--------

This project uses sbt and requires node.js to be installed in order to run Scala.js tests. To build, run `sbt publishLocal`.

Code of Conduct
---------------

See the [Code of Conduct](CODE_OF_CONDUCT.md).

