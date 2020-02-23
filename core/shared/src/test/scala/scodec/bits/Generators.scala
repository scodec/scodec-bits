package scodec.bits

import hedgehog._
import java.nio.ByteBuffer

object Generators {
//   implicit val arbitraryBitVector: Arbitrary[BitVector] = Arbitrary {
//     Gen.oneOf(flatBytes, balancedTrees, splitVectors, concatSplitVectors, bitStreams)
//   }
//   val flatBytes = genBitVector(500, 7) // regular flat bit vectors
//   val balancedTrees = genConcatBits(genBitVector(2000, 7)) // balanced trees of concatenations
//   val splitVectors = genSplitBits(5000) // split bit vectors: b.take(m) ++ b.drop(m)
//   val concatSplitVectors = genConcatBits(genSplitBits(5000)) // concatenated split bit vectors

//   val chunk = Gen.oneOf(flatBytes, balancedTrees, splitVectors, concatSplitVectors)
//   val bitStreams = genBitStream(chunk, Gen.choose(0, 5)) // streams of chunks

//   def genSplitBits(maxSize: Long) =
//     for {
//       n <- Gen.choose(0L, maxSize)
//       b <- genBitVector(15, 7)
//     } yield {
//       val m = if (b.nonEmpty) (n % b.size).abs else 0
//       b.take(m) ++ b.drop(m)
//     }

//   def genConcatBits(g: Gen[BitVector]) =
//     g.map { b =>
//       b.toIndexedSeq.foldLeft(BitVector.empty)((acc, high) => acc ++ BitVector.bit(high))
//     }


  def genBitVector: Gen[BitVector] = genSimpleBitVector()

  private def genSimpleBitVector(maxBytes: Int = 1024, maxAdditionalBits: Int = 7): Gen[BitVector] =
    for {
      byteSize <- Gen.int(Range.linear(0, maxBytes))
      additionalBits <- Gen.int(Range.linear(0, maxAdditionalBits))
      size = byteSize * 8 + additionalBits
      bytes <- genByteVectorArrayView((size + 7) / 8)
    } yield BitVector(bytes).take(size.toLong)

  def genUnfoldedBitVector(g: Gen[BitVector], steps: Range[Int]): Gen[BitVector] =
    g.list(steps).map(chunks => BitVector.unfold(chunks)(s => s.headOption.map((_, s.tail))))

  // very deeply right nested - to check for SOE
  val genHugeBitVectors: Gen[BitVector] =
    genUnfoldedBitVector(genSimpleBitVector(30, 7), Range.linear(4500, 5000))

  def genByte: Gen[Byte] = Gen.byte(Range.constantFrom(0, Byte.MinValue, Byte.MaxValue))

  private def genByteArray(maxSize: Int): Gen[Array[Byte]] = Gen.list(genByte, Range.linear(0, maxSize)).map(_.toArray)

  def genByteVector: Gen[ByteVector] = Gen.choice1(
    genByteVectorArrayView(100),
    genConcatByteVectors(genByteVectorArrayView(100)),
    genSliceByteVectors,
    genSplitByteVectors(genSliceByteVectors),
    genSplitByteVectors(genConcatByteVectors(genByteVectorArrayView(500))),
    genByteBufferVectors(100)
  )

  private def genByteVectorArrayView(maxSize: Int): Gen[ByteVector] =
    genByteArray(maxSize).map(ByteVector.view)

  private def genConcatByteVectors(g: Gen[ByteVector]): Gen[ByteVector] =
    g.map(b => b.foldLeft(ByteVector.empty)(_ :+ _))

  private def genSliceByteVectors: Gen[ByteVector] =
    for {
      b <- genByteVectorArrayView(100)
      toDrop <- Gen.long(Range.linear(0, b.size))
    } yield b.drop(toDrop)

  private def genSplitByteVectors(g: Gen[ByteVector]): Gen[ByteVector] =
    for {
      b <- g
      n <- Gen.long(Range.linear(0, b.size))
    } yield b.take(n) ++ b.drop(n)

  private def genByteBufferVectors(maxSize: Int): Gen[ByteVector] =
    genByteArray(maxSize).map(arr => ByteVector.view(ByteBuffer.wrap(arr)))

  def genVeryLargeByteVectors: Gen[ByteVector] =
    genByte.map(b => ByteVector.fill(Int.MaxValue.toLong + 1)(b))
}