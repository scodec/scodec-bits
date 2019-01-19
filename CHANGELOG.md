1.1.9
=====
 - Restored JDK8 compatibility

1.1.8
=====
 - Fixed a bug in `sliceToByte` when the offset was non-zero (https://github.com/scodec/scodec-bits/issues/105)
 - Changed the return type of `grouped` from `Stream` to `Iterator` (https://github.com/scodec/scodec-bits/issues/55)
 - Improved clarity of error message in `padLeft` / `padRight` (https://github.com/scodec/scodec-bits/issues/104)
 - Improved Scala 2.13 compatibility by avoiding deprecated collection types in signatures (https://github.com/scodec/scodec-bits/issues/108)

1.1.6
=====
 - Fixed `BitVector.highByte` to return the correct value (https://github.com/scodec/scodec/issues/108)
 - Fixed bug in `ByteVector#equals` which would through an `IllegalArgumentException` if the vector had more than `Int.MaxValue` elements (https://github.com/scodec/scodec-bits/issues/90)

1.1.5
=====
 - Added support for Scala Native
 - Added support for Scala 2.13.0-M1
 - Fixed `fromBase64` variants to handle malformed input better (https://github.com/scodec/scodec-bits/issues/76)

1.1.4
=====
 - Fixed bug in `ByteVector.view` when passing a `java.nio.ByteBuffer` with a non-zero position
 - Significantly improved performance of `BitVector#reverse` and added `BitVector.reverseBitsInByte`

1.1.3
=====
 - Added `copyToBuffer` to `ByteVector`

1.1.2
=====
 - Fixed bug in `slice` on `BitVector`/`ByteVector` when passed negative indices
 - Added an extractor for `ByteVector`, allowing pattern matching on the bytes that make up a vector
 - Added an overload of `ByteVector.view` that allows viewing a slice of an array
 - Added UUID conversions for `BitVector`/`ByteVector`
 - Improved performance of 32-bit CRCs by a factory of 10

1.1.1
=====
 - Added `zipWith2` and `zipWith3` to `ByteVector`
 - Added `foldLeftBB` and `foldRightBB` to `ByteVector`, allowing zero-copy folds
 - Fixed stack overflow in `BitVector#hashCode` and `BitVector#suspend` on very large vectors with lots of append/suspend nodes

1.1.0
=====
 - Changed `ByteVector` to be `Long` indexed instead of `Int` indexed.
 - Renamed `ByteVector.view(Long => Byte, Long)` to `ByteVector.viewAt`.

1.0.12
======
 - Fix correctness bug in `ByteVector.fromBase64` when string contained ignored characters.

1.0.11
======
 - Improved performance of CRCs -- 32-bit CRCs improved by 600% and n-bit CRCs improved by 40%.

1.0.10
======
 - Improved performance of `toInt` and `fromInt`, `toLong` and `fromLong`, etc.
 - Further performance improvements in `toBase64`.
 - Improved performance of fromBase64 by over 200x
 - Added type-safe === to `BitVector` and `ByteVector`

1.0.9
=====
 - *Significant* performance improvement in `toBase64`.
 - Fixed source links in ScalaDoc.

1.0.7
=====
 - Improved performance of creating vectors from hex/bin.

1.0.6
=====
 - Added `deflate` and `inflate` to both `BitVector` and `ByteVector`, which compresses the vector with ZLIB.
 - Added `encrypt` and `decrypt` to both `BitVector` and `ByteVector`.
 - Added `takeWhile` and `dropWhile` to `ByteVector`.
 - Added 4-argument overload of `copyToArray` to `ByteVector`.
 - Added ability to encode/decode stings to/from `BitVector`s and `ByteVector`s.

1.0.5
=====
 - Added `concat` to the companions of `BitVector` and `ByteVector`.
 - Removed scala-reflect library from transitive dependencies.
 - Changed organization (groupId) from `org.typelevel` to `org.scodec`.

1.0.4
=====
 - Significant performance improvements in `BitVector`
   See [https://github.com/scodec/scodec-bits/pull/21] for details.
 - Added `toByte`/`sliceToByte` and `toShort`/`sliceToShort` to `BitVector` and `ByteVector`
 - Fixed bug in `toInt`/`toLong` on bit vectors with sizes not evenly divisible by 8

1.0.3
=====
 - Significant performance improvements in `:+` and `++` for `ByteVector` and `BitVector`.
   See [https://github.com/scodec/scodec-bits/pull/16] and [https://github.com/scodec/scodec-bits/pull/19] for details.

1.0.2
=====
 - Published ScalaDoc links in POM so that other projects can link to ScalaDoc

1.0.1
=====
 - Added toInt/toLong/fromInt/fromLong to BitVector and ByteVector
 - Changed toString of empty BitVector/ByteVector
 - More performant versions of ByteVector#toArray and ByteVector#toByteBuffer
 - Added ByteVector#copyToArray and copyToStream

1.0.0
=====
 - Added `scodec.bits.crc` for calculating cyclic redundancy checks
 - Improved performance of `drop`
 - Added `digest` to `BitVector` and `ByteVector`
 - Renamed `padToRight` to `padLeft` and aliased `padTo` to `padRight`
 - Renamed `leftShift/rightShift` to `shiftLeft/shiftRight`
 - Added `rotateLeft` and `rotateRight`
 - Added serialization support for `BitVector` and `ByteVector`

1.0.0-RC2
=========
 - Added `BitVector#bytes` as alias for `toByteVector` and `ByteVector#bits` as alias for `toBitVector`
 - Improved performance of `sizeLessThan` and `sizeGreaterThan`

1.0.0-RC1
=========
 - Much more performant implementation of ByteVector by Paul C. that implements various operations in constant/log time
 - General performance improvements, including stack safe versions of many methods
 - Base 64 encoding/decoding
 - Revamped base encoding scheme that allows pluggable alphabets (e.g., uppercase hex, truthy binary, base64 url compat)
 - Many new collection like methods (e.g., headOption, init, last, lastOption, nonEmpty, padToRight, startsWith, endsWith, indexOfSlice, containsSlice, splice, patch, populationCount)
 - Reduced public API footprint
 - API docs

1.0.0-M2
========
 - Changed group id from com.github.scodec to org.typelevel
 - Added convenience constructors to `BitVector` and `ByteVector` from `GenTraversableOnce`
 - Fixed various bugs in SliceByteVector
 - Performance improvements

1.0.0-M1
========
 - Initial separation from scodec-core project
 - Much more performant implementation of BitVector by Paul C. that implements various operations in constant time
 - Lazy implementations of `ByteVector` by Paul C. that allow working performantly with Java IO/NIO
