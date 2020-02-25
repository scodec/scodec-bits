package scodec.bits

import hedgehog._

import java.nio.ByteBuffer
import java.util.UUID

object Generators {

  def genBitVector: Gen[BitVector] =
    Gen.choice1(
      genSimpleBitVector(500, 7),
      genConcatBitVector(genSimpleBitVector(2000, 7)),
      genSplitBitVector,
      genConcatBitVector(genSplitBitVector),
      genUnfoldedBitVector(
        Gen.choice1(
          genSimpleBitVector(500, 7),
          genConcatBitVector(genSimpleBitVector(2000, 7)),
          genSplitBitVector,
          genConcatBitVector(genSplitBitVector)
        ),
        Range.linear(0, 5)
      )
    )
  genSimpleBitVector()

  def genSimpleBitVector(maxBytes: Int = 1024, maxAdditionalBits: Int = 7): Gen[BitVector] =
    for {
      byteSize <- Gen.int(Range.linear(0, maxBytes))
      additionalBits <- Gen.int(Range.linear(0, maxAdditionalBits))
      size = byteSize * 8 + additionalBits
      bytes <- genByteVectorArrayView((size + 7) / 8)
    } yield BitVector(bytes).take(size.toLong)

  private def genUnfoldedBitVector(g: Gen[BitVector], steps: Range[Int]): Gen[BitVector] =
    g.list(steps).map(chunks => BitVector.unfold(chunks)(s => s.headOption.map((_, s.tail))))

  private def genConcatBitVector(g: Gen[BitVector]): Gen[BitVector] =
    Gen.list(g, Range.linear(0, 10)).map(bs => bs.foldLeft(BitVector.empty)(_ ++ _))

  private def genSplitBitVector: Gen[BitVector] =
    for {
      b <- genSimpleBitVector(15, 7)
      n <- Gen.long(Range.linear(0, b.size))
    } yield b.take(n) ++ b.drop(n)

  // very deeply right nested - to check for SOE
  val genHugeBitVectors: Gen[BitVector] =
    // genUnfoldedBitVector(genSimpleBitVector(30, 7), Range.constant(4500, 5000)) - TODO SOE - https://github.com/hedgehogqa/scala-hedgehog/issues/47
    genUnfoldedBitVector(genSimpleBitVector(30, 7), Range.constant(450, 500))

  def genByte: Gen[Byte] = Gen.byte(Range.constantFrom(0, Byte.MinValue, Byte.MaxValue))

  def genByteArray(maxSize: Int): Gen[Array[Byte]] =
    Gen.list(genByte, Range.linear(0, maxSize)).map(_.toArray)

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

  def genUUID: Gen[UUID] =
    for {
      upper <- Gen.long(Range.constantFrom(0, Long.MinValue, Long.MaxValue))
      lower <- Gen.long(Range.constantFrom(0, Long.MinValue, Long.MaxValue))
    } yield new UUID(upper, lower)
}
