package scodec.bits

import java.security.MessageDigest
import org.scalacheck.{Arbitrary, Gen, Shrink}
import Arbitrary.arbitrary
import java.nio.ByteBuffer
import java.io.ByteArrayOutputStream

class ByteVectorTest extends BitsSuite {

  def standardByteVectors(maxSize: Int): Gen[ByteVector] = for {
    size <- Gen.choose(0, maxSize)
    bytes <- Gen.listOfN(size, Gen.choose(0, 255))
  } yield ByteVector(bytes: _*)

  val sliceByteVectors: Gen[ByteVector] = for {
    bytes <- arbitrary[Array[Byte]]
    toDrop <- Gen.choose(0, bytes.size)
  } yield ByteVector.view(bytes).drop(toDrop)

  def genSplit(g: Gen[ByteVector]) = for {
    b <- g
    n <- Gen.choose(0L, b.size+1)
  } yield {
    b.take(n) ++ b.drop(n)
  }

  def genByteBufferVectors(maxSize: Int): Gen[ByteVector] = for {
    size <- Gen.choose(0, maxSize)
    bytes <- Gen.listOfN(size, Gen.choose(0, 255))
  } yield ByteVector.view(ByteBuffer.wrap(bytes.map(_.toByte).toArray))

  def genConcat(g: Gen[ByteVector]) =
    g.map { b => b.toIndexedSeq.foldLeft(ByteVector.empty)(_ :+ _) }

  val byteVectors: Gen[ByteVector] = Gen.oneOf(
    standardByteVectors(100),
    genConcat(standardByteVectors(100)),
    sliceByteVectors,
    genSplit(sliceByteVectors),
    genSplit(genConcat(standardByteVectors(500))),
    genByteBufferVectors(100))

  val bytesWithIndex = for {
    b <- byteVectors
    i <- Gen.choose(0L, b.size+1)
  } yield (b, i)

  implicit val arbitraryByteVectors: Arbitrary[ByteVector] = Arbitrary(byteVectors)

  implicit val shrinkByteVector: Shrink[ByteVector] =
    Shrink[ByteVector] { b =>
      if (b.nonEmpty)
        Stream.iterate(b.take(b.size / 2))(b2 => b2.take(b2.size / 2)).takeWhile(_.nonEmpty) ++ Stream(ByteVector.empty)
      else Stream.empty
    }

  test("hashCode/equals") {
    forAll (bytesWithIndex) { case (b, m) =>
      (b.take(m) ++ b.drop(m)) shouldBe b
      (b.take(m) ++ b.drop(m)).hashCode shouldBe b.hashCode
      if (b.take(3) == b.drop(3).take(3)) {
        // kind of weak, since this will only happen 1/8th of attempts on average
        b.take(3).hashCode shouldBe b.drop(3).take(3).hashCode
      }
    }
  }

  test("compact is a no-op for already compact byte vectors") {
    val b = ByteVector(0x80)
    (b.compact eq b.compact) shouldBe true
  }

  test("reverse.reverse == id") {
    forAll { (b: ByteVector) => b.reverse.reverse shouldBe b }
  }

  test("foldRight/left") {
    forAll { (b: ByteVector) => b.foldLeft(ByteVector.empty)(_ :+ _) shouldBe b }
    forAll { (b: ByteVector) => b.foldRight(ByteVector.empty)(_ +: _) shouldBe b }
  }

  test("insert") {
    val b = ByteVector.empty
    b.insert(0, 1) shouldBe ByteVector(1)
    ByteVector(1,2,3,4).insert(0, 0) shouldBe ByteVector(0,1,2,3,4)
    ByteVector(1,2,3,4).insert(1, 0) shouldBe ByteVector(1,0,2,3,4)
    forAll { (b: ByteVector) =>
      b.foldLeft(ByteVector.empty)((acc,b) => acc.insert(acc.size, b)) shouldBe b
    }
  }

  test("consistent with Array[Byte] implementations") {
    forAll (bytesWithIndex) { case (b, ind) =>
      val indI = ind.toInt
      val ba = b.toArray
      b.take(ind).toArray shouldBe ba.take(indI)
      b.drop(ind).toArray shouldBe ba.drop(indI)
      b.lift(ind) shouldBe ba.lift(indI)
      b.takeRight(ind).toArray shouldBe ba.takeRight(indI)
      b.dropRight(ind).toArray shouldBe ba.dropRight(indI)
      b.reverse.toArray shouldBe ba.reverse
      b.partialCompact(ind).toArray shouldBe ba
      b.lastOption shouldBe ba.lastOption
      b.nonEmpty shouldBe ba.nonEmpty
      if (b.nonEmpty) {
        b.last shouldBe ba.last
        b.init.toArray shouldBe ba.init
      }
      if (ind < b.size) {
        val actual = b.update(ind,9).toArray
        val correct = Vector(b.toArray: _*).updated(indI,9).toArray
        actual shouldBe correct
      }

    }
    forAll { (b1: ByteVector, b2: ByteVector) =>
      (b1 ++ b2).toArray shouldBe (b1.toArray ++ b2.toArray)
    }
  }

  val deadbeef = ByteVector(0xde, 0xad, 0xbe, 0xef)

  test("toHex") {
    deadbeef.toHex shouldBe "deadbeef"
  }

  test("fromHexDescriptive") {
    ByteVector.fromHexDescriptive("0xdeadbeef") shouldBe Right(deadbeef)
    ByteVector.fromHexDescriptive("0xDEADBEEF") shouldBe Right(deadbeef)
    ByteVector.fromHexDescriptive("0XDEADBEEF") shouldBe Right(deadbeef)
    ByteVector.fromHexDescriptive("deadbeef") shouldBe Right(deadbeef)
    ByteVector.fromHexDescriptive("DEADBEEF") shouldBe Right(deadbeef)
    ByteVector.fromHexDescriptive("de ad be ef") shouldBe Right(deadbeef)
    ByteVector.fromHexDescriptive("de\tad\nbe\tef") shouldBe Right(deadbeef)
    ByteVector.fromHexDescriptive("0xde_ad_be_ef") shouldBe Right(deadbeef)

    ByteVector.fromHexDescriptive("0xdeadbee") shouldBe Right(ByteVector(0x0d, 0xea, 0xdb, 0xee))
    ByteVector.fromHexDescriptive("0xde_ad_be_e") shouldBe Right(ByteVector(0x0d, 0xea, 0xdb, 0xee))

    ByteVector.fromHexDescriptive("garbage") shouldBe Left("Invalid hexadecimal character 'g' at index 0")
    ByteVector.fromHexDescriptive("deadbefg") shouldBe Left("Invalid hexadecimal character 'g' at index 7")
  }

  test("toBin") {
    deadbeef.toBin shouldBe "11011110101011011011111011101111"
  }

  test("fromBinDescriptive") {
    ByteVector.fromBinDescriptive(deadbeef.toBin) shouldBe Right(deadbeef)
    ByteVector.fromBinDescriptive(deadbeef.toBin.grouped(4).mkString(" ")) shouldBe Right(deadbeef)
    ByteVector.fromBinDescriptive("0001 0011") shouldBe Right(ByteVector(0x13))
    ByteVector.fromBinDescriptive("0b 0001 0011 0111") shouldBe Right(ByteVector(0x01, 0x37))
    ByteVector.fromBinDescriptive("1101a000") shouldBe Left("Invalid binary character 'a' at index 4")
    ByteVector.fromBinDescriptive("0b1101a000") shouldBe Left("Invalid binary character 'a' at index 6")
    ByteVector.fromBinDescriptive("0B1101a000") shouldBe Left("Invalid binary character 'a' at index 6")
  }

  test("fromValidBin") {
    ByteVector.fromValidBin(deadbeef.toBin) shouldBe deadbeef
    an[IllegalArgumentException] should be thrownBy { ByteVector.fromValidBin("1101a000") }
  }

  test("toBase64") {
    forAll { (b: ByteVector) =>
      val guavaB64 = com.google.common.io.BaseEncoding.base64
      ByteVector.view(guavaB64.decode(b.toBase64)) shouldBe b
    }
  }

  test("fromBase64") {
    forAll { (b: ByteVector) =>
      val guavaB64 = com.google.common.io.BaseEncoding.base64
      ByteVector.fromValidBase64(guavaB64.encode(b.toArray)) shouldBe b
    }
  }

  test("buffer :+") {
    forAll { (b: ByteVector, bs: List[ByteVector], n: Int) => {
      val unbuf = bs.foldLeft(b)(_ ++ _)
      val buf = bs.foldLeft(b.bufferBy((n % 50).max(0) + 1))((acc,a) =>
        a.foldLeft(acc)(_ :+ _)
      )
      unbuf shouldBe buf
      (0L until unbuf.size).foreach { i => unbuf(i) shouldBe buf(i) }
    }}
  }

  test("buffer ++/take/drop") {
    forAll { (b: ByteVector, bs: List[ByteVector], n: Int) =>
      val unbuf = bs.foldLeft(b)(_ ++ _)
      val buf = bs.foldLeft(b.bufferBy((n % 50).max(0) + 1))(_ ++ _)
      unbuf shouldBe buf
      (0L until unbuf.size).foreach { i => unbuf(i) shouldBe buf(i) }
      val ind = (n % (unbuf.size+1)).max(0) + 1
      buf.take(ind) shouldBe unbuf.take(ind)
      buf.drop(ind) shouldBe unbuf.drop(ind)
    }
  }

  test("buffer concurrency") {
    import java.util.concurrent.Callable
    val pool = java.util.concurrent.Executors.newFixedThreadPool(4)

    // Concurrently append b1.buffer ++ b2 and b1.buffer ++ b3
    // making sure this gives same results as unbuffered appends
    forAll { (b1: ByteVector, b2: ByteVector, b3: ByteVector, n: Int) =>
      val b1b = b1.bufferBy((n % 50).max(0) + 1)
      val b1b2 = new Callable[ByteVector] { def call = b1b ++ b2 }
      val b1b3 = new Callable[ByteVector] { def call = b1b ++ b3 }
      val rb1b2 = pool.submit(b1b2)
      val rb1b3 = pool.submit(b1b3)
      rb1b2.get shouldBe (b1 ++ b2)
      rb1b3.get shouldBe (b1 ++ b3)
    }
    pool.shutdown
  }

  test("buffer rebuffering") {
    forAll { (b1: ByteVector, b2: ByteVector, b3: ByteVector, n: Int) =>
      val chunkSize = (n % 50).max(0) + 1
      val b1b = b1.bufferBy(chunkSize)
      val b1b2b3 = (b1b ++ b2).bufferBy(chunkSize + 1) ++ b3
      b1b2b3 shouldBe (b1 ++ b2 ++ b3)
    }
  }

  test("<<") {
    ByteVector(0x55, 0x55, 0x55) << 1 shouldBe ByteVector(0xaa, 0xaa, 0xaa)
  }

  test(">>") {
    ByteVector(0x55, 0x55, 0x55) >> 1 shouldBe ByteVector(0x2a, 0xaa, 0xaa)
    ByteVector(0xaa, 0xaa, 0xaa) >> 1 shouldBe ByteVector(0xd5, 0x55, 0x55)
  }

  test(">>>") {
    ByteVector(0x55, 0x55, 0x55) >>> 1 shouldBe ByteVector(0x2a, 0xaa, 0xaa)
    ByteVector(0xaa, 0xaa, 0xaa) >>> 1 shouldBe ByteVector(0x55, 0x55, 0x55)
  }

  test("rotations") {
    forAll { (b: ByteVector, n: Int) =>
      b.rotateLeft(b.size * 8) shouldBe b
      b.rotateRight(b.size * 8) shouldBe b
      b.rotateRight(n).rotateLeft(n) shouldBe b
      b.rotateLeft(n).rotateRight(n) shouldBe b
    }
  }

  test("hex string interpolator") {
    hex"deadbeef" shouldBe deadbeef
    val x = ByteVector.fromValidHex("be")
    hex"dead${x}ef" shouldBe deadbeef
    """hex"deadgg"""" shouldNot compile
  }

  test("toIterable roundtrip") {
    forAll { (b: ByteVector) =>
      val fromIter = ByteVector(b.toIterable)
      b shouldBe fromIter
      fromIter shouldBe b
    }
  }

  test("toArray roundtrip") {
    forAll { (b: ByteVector) =>
      val fromArr = ByteVector(b.toArray)
      b shouldBe fromArr
      fromArr shouldBe b
      // Ensure immutable behavior
      val fromArr2 = ByteVector(b.toArray)
      fromArr shouldBe fromArr2
    }
  }

  test("copyToStream roundtrip") {
    forAll { (b: ByteVector) =>
      val os = new ByteArrayOutputStream()
      b.copyToStream(os)
      val fromArr = ByteVector(os.toByteArray)
      b shouldBe fromArr
      fromArr shouldBe b
    }
  }

  test("toByteBuffer roundtrip") {
    forAll { (b: ByteVector) =>
      val fromBuffer = ByteVector(b.toByteBuffer)
      b shouldBe fromBuffer
      fromBuffer shouldBe b
    }
  }

  test("dropping from a view is consistent with dropping from a strict vector") {
    forAll { (b: ByteVector, n0: Int) =>
      val view = ByteVector.view(b.toArray)
      val n = n0.abs
      b.drop(n) shouldBe view.drop(n)
    }
  }

  test("grouped + concatenate") {
    forAll { (bv: ByteVector) =>
      if (bv.isEmpty) {
        bv.grouped(1) shouldBe Stream.empty
      } else if (bv.size < 3) {
        bv.grouped(bv.size.toInt) shouldBe Stream(bv)
      } else {
        bv.grouped(bv.size.toInt / 3).toList.foldLeft(ByteVector.empty) { (acc, b) => acc ++ b } shouldBe bv
      }
    }
  }

  test("indexOfSlice/containsSlice/startsWith") {
    forAll { (bv: ByteVector, m0: Int, n0: Int) =>
      val m = if (bv.nonEmpty) (m0 % bv.size).abs else 0
      val n = if (bv.nonEmpty) (n0 % bv.size).abs else 0
      val slice = bv.slice(m min n, m max n)
      val idx = bv.indexOfSlice(slice)
      idx shouldBe bv.toIndexedSeq.indexOfSlice(slice.toIndexedSeq)
      bv.containsSlice(slice) shouldBe true
      if (bv.nonEmpty) bv.containsSlice(bv ++ bv) shouldBe false
    }
  }

  test("endsWith") {
    forAll { (bv: ByteVector, n0: Int) =>
      val n = if (bv.nonEmpty) (n0 % bv.size).abs else 0
      val slice = bv.takeRight(n)
      bv.endsWith(slice) shouldBe true
      if (slice.nonEmpty) bv.endsWith(~slice) shouldBe false
    }
  }

  test("splice") {
    forAll { (x: ByteVector, y: ByteVector, n0: Int) =>
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0
      x.splice(n, ByteVector.empty) shouldBe x
      x.splice(n, y) shouldBe (x.take(n) ++ y ++ x.drop(n))
    }
  }

  test("patch") {
    forAll { (x: ByteVector, y: ByteVector, n0: Int) =>
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0
      x.patch(n, x.slice(n, n)) shouldBe x
      x.patch(n, y) shouldBe (x.take(n) ++ y ++ x.drop(n + y.size))
    }
  }

  test("int conversions") {
    forAll { (n: Int) =>
      ByteVector.fromInt(n).toInt() shouldBe n
      ByteVector.fromInt(n, ordering = ByteOrdering.LittleEndian).toInt(ordering = ByteOrdering.LittleEndian) shouldBe n
    }
  }

  test("long conversions") {
    forAll { (n: Int) =>
      ByteVector.fromLong(n).toLong() shouldBe n
      ByteVector.fromLong(n, ordering = ByteOrdering.LittleEndian).toLong(ordering = ByteOrdering.LittleEndian) shouldBe n
    }
  }

  test("digest") {
    forAll { (x: ByteVector) =>
      val sha256 = MessageDigest.getInstance("SHA-256")
      x.digest("SHA-256") shouldBe ByteVector.view(sha256.digest(x.toArray))
    }
  }

  test("serialization") {
    forAll { (x: ByteVector) => serializationShouldRoundtrip(x) }
  }
}
