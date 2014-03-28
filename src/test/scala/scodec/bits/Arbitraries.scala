package scodec.bits

import org.scalacheck.{Arbitrary, Gen, Shrink}

object Arbitraries {

  val flatBytes = genBitVector(500, 7)                 // regular flat bit vectors
  val balancedTrees = genConcat(genBitVector(2000, 7)) // balanced trees of concatenations
  val splitVectors = genSplit(5000)                    // split bit vectors: b.take(m) ++ b.drop(m)
  val concatSplitVectors = genConcat(genSplit(5000))   // concatenated split bit vectors

  val chunk = Gen.oneOf(flatBytes, balancedTrees, splitVectors, concatSplitVectors)
  val bitStreams = genBitStream(chunk, Gen.choose(0,5)) // streams of chunks

  // very deeply right nested - to check for SOE
  val hugeBitStreams = genBitStream(
    genBitVector(30,7),
    Gen.choose(1000,5000))

  implicit val shrinkBitVector: Shrink[BitVector] =
    Shrink[BitVector] { b =>
      if (b.nonEmpty)
        Stream.iterate(b.take(b.size / 2))(b2 => b2.take(b2.size / 2)).takeWhile(_.nonEmpty) ++
        Stream(BitVector.empty)
      else Stream.empty
    }

  def genBitStream(g: Gen[BitVector], steps: Gen[Int]): Gen[BitVector] = for {
    n <- steps
    chunks <- Gen.listOfN(n, g)
  } yield BitVector.unfold(chunks)(s => s.headOption.map((_,s.tail)))

  def genBitVector(maxBytes: Int = 1024, maxAdditionalBits: Int = 7): Gen[BitVector] = for {
    byteSize <- Gen.choose(0, maxBytes)
    additionalBits <- Gen.choose(0, maxAdditionalBits)
    size = byteSize * 8 + additionalBits
    bytes <- Gen.listOfN((size + 7) / 8, Gen.choose(0, 255))
  } yield BitVector(ByteVector(bytes: _*)).take(size)

  def genSplit(maxSize: Long) = for {
    n <- Gen.choose(0L, maxSize)
    b <- genBitVector(15, 7)
  } yield {
    val m = if (b.nonEmpty) (n % b.size).abs else 0
    b.take(m) ++ b.drop(m)
  }

  def genConcat(g: Gen[BitVector]) =
    genBitVector(2000, 7).map { b =>
      b.toIndexedSeq.foldLeft(BitVector.empty)(
        (acc,high) => acc ++ BitVector.bit(high)
      )
    }
}
