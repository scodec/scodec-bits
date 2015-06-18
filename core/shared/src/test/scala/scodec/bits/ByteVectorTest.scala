package scodec.bits

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import java.io.ByteArrayOutputStream
import org.scalatest.Matchers._

import Arbitraries._

class ByteVectorTest extends BitsSuite {

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
      val ba = b.toArray
      b.take(ind).toArray shouldBe ba.take(ind)
      b.drop(ind).toArray shouldBe ba.drop(ind)
      b.lift(ind) shouldBe ba.lift(ind)
      b.takeRight(ind).toArray shouldBe ba.takeRight(ind)
      b.dropRight(ind).toArray shouldBe ba.dropRight(ind)
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
        val correct = Vector(b.toArray: _*).updated(ind, 9.toByte).toArray
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
    an[IllegalArgumentException] should be thrownBy { ByteVector.fromValidBin("1101a000"); () }
  }

  test("base64 roundtrip") {
    forAll { (b: ByteVector) =>
      ByteVector.fromValidBase64(b.toBase64) shouldBe b
    }
  }

  test("buffer :+") {
    forAll { (b: ByteVector, bs: List[ByteVector], n: Int) => {
      val unbuf = bs.foldLeft(b)(_ ++ _)
      val buf = bs.foldLeft(b.bufferBy((n % 50).max(0) + 1))((acc,a) =>
        a.foldLeft(acc)(_ :+ _)
      )
      unbuf shouldBe buf
      (0 until unbuf.size).foreach { i => unbuf(i) shouldBe buf(i) }
    }}
  }

  test("buffer ++/take/drop") {
    forAll { (b: ByteVector, bs: List[ByteVector], n: Int) =>
      val unbuf = bs.foldLeft(b)(_ ++ _)
      val buf = bs.foldLeft(b.bufferBy((n % 50).max(0) + 1))(_ ++ _)
      unbuf shouldBe buf
      (0 until unbuf.size).foreach { i => unbuf(i) shouldBe buf(i) }
      val ind = (n % (unbuf.size+1)).max(0) + 1
      buf.take(ind) shouldBe unbuf.take(ind)
      buf.drop(ind) shouldBe unbuf.drop(ind)
    }
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
        bv.grouped(bv.size) shouldBe Stream(bv)
      } else {
        bv.grouped(bv.size / 3).toList.foldLeft(ByteVector.empty) { (acc, b) => acc ++ b } shouldBe bv
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

  test("short conversions") {
    forAll { (n: Short) =>
      ByteVector.fromShort(n).toShort() shouldBe n
      ByteVector.fromShort(n, ordering = ByteOrdering.LittleEndian).toShort(ordering = ByteOrdering.LittleEndian) shouldBe n
    }
  }

  test("int conversions") {
    forAll { (n: Int) =>
      ByteVector.fromInt(n).toInt() shouldBe n
      ByteVector.fromInt(n, ordering = ByteOrdering.LittleEndian).toInt(ordering = ByteOrdering.LittleEndian) shouldBe n
    }
  }

  test("long conversions") {
    forAll { (n: Long) =>
      ByteVector.fromLong(n).toLong() shouldBe n
      ByteVector.fromLong(n, ordering = ByteOrdering.LittleEndian).toLong(ordering = ByteOrdering.LittleEndian) shouldBe n
    }
  }

  test("concat") {
    forAll { (bvs: List[ByteVector]) =>
      val c = ByteVector.concat(bvs)
      c.size shouldBe bvs.map(_.size).foldLeft(0)(_ + _)
      bvs.headOption.foreach(h => c.startsWith(h))
      bvs.lastOption.foreach(l => c.endsWith(l))
    }
  }

  test("copyToArray with offset/size") {
    forAll { (b: ByteVector) =>
      val size = b.size / 3
      val start = b.size / 4
      val offset = b.size / 5
      val xs = new Array[Byte](b.size)
      b.copyToArray(xs, start, offset, size)
      xs shouldBe (xs.take(start) ++ b.drop(offset).take(size).toArray ++ xs.drop(start + size)).toArray
    }
  }

  test("dropWhile") {
    forAll { (x: ByteVector) =>
      val (expected, _) = x.foldLeft((ByteVector.empty, true)) { case ((acc, dropping), b) =>
        if (dropping) {
          if (b == 0) (acc :+ 0, false)
          else (acc, true)
        } else {
          (acc :+ b, false)
        }
      }
      x.dropWhile(_ != 0.toByte) shouldBe expected
    }
  }

  test("takeWhile") {
    forAll { (x: ByteVector) =>
      val (expected, _) = x.foldLeft((ByteVector.empty, true)) { case ((acc, taking), b) =>
        if (taking) {
          if (b == 0) (acc, false)
          else (acc :+ b, true)
        } else {
          (acc, false)
        }
      }
      x.takeWhile(_ != 0.toByte) shouldBe expected
    }
  }
}
