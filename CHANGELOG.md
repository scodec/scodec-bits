1.0.0-M2
========
 - Changed group id from com.github.scodec to org.typelevel
 - Added convenience constructors to `BitVector` and `ByteVector` from `GenTraversableOnce`

1.0.0-M1
========
 - Initial separation from scodec-core project
 - Much more performant implementation of BitVector by Paul C. that implements various operations in constant time
 - Lazy implementations of `ByteVector` by Paul C. that allow working performantly with Java IO/NIO
