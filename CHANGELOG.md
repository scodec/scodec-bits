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
