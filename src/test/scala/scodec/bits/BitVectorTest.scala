package scodec.bits

import java.security.MessageDigest
import org.scalacheck.{Arbitrary, Gen}
import Arbitraries._

class BitVectorTest extends BitsSuite {
  implicit val arbitraryBitVector: Arbitrary[BitVector] = Arbitrary {
    Gen.oneOf(flatBytes, balancedTrees, splitVectors, concatSplitVectors, bitStreams)
  }

  test("hashCode/equals") {
    forAll { (b: BitVector, b2: BitVector, m: Long) =>
      (b.take(m) ++ b.drop(m)).hashCode shouldBe b.hashCode
      if (b.take(3) == b2.take(3)) {
        // kind of weak, since this will only happen 1/8th of attempts on average
        b.take(3).hashCode shouldBe b2.take(3).hashCode
      }
    }
  }

  test("compact is a no-op for already compact bit vectors") {
    val b = BitVector(0x80,0x90)
    (b.compact.underlying eq b.compact.underlying) shouldBe true
    (b.toByteVector eq b.toByteVector) shouldBe true
    val b2 = b.drop(8) // also make sure this works if we're talking about a byte-aligned slice
    (b2.compact.underlying eq b2.compact.underlying) shouldBe true
    (b2.toByteVector eq b2.toByteVector) shouldBe true
  }

  test("hashCode/equals/take/drop stack safety") {
    forAll (hugeBitStreams) { b =>
      b shouldBe b // this exercises take/drop
      b.hashCode shouldBe b.hashCode
    }
  }

  test("acquire stack safety for lazy BitVector") {
    val nats = BitVector.unfold(0)(i => Some(BitVector.high(1000) -> (i+1)))
    nats.acquire(100000).isRight shouldBe true
  }

  val bitVectorWithTakeIndex = Arbitrary.arbitrary[BitVector].flatMap { bits =>
    Gen.choose(0L, bits.size+1) map ((bits,_))
  }

  test("acquire/take consistency") {
    def check(bits: BitVector, n: Long): Unit =
      bits.acquire(n) match {
        case Left(_) => bits.size < n
        case Right(hd) => hd shouldBe bits.take(n)
      }

    forAll (bitVectorWithTakeIndex) { case (bits, ind) =>
      check(bits, ind)
      check(bits, ind*2)
    }
  }

  test("1-bit vectors") {
    BitVector.zero.head shouldBe false
    BitVector.one.head shouldBe true
    BitVector.bit(false).head shouldBe false
    BitVector.bit(true).head shouldBe true
  }

  test("construction via high") {
    BitVector.high(1).toByteVector shouldBe ByteVector(0x80)
    BitVector.high(2).toByteVector shouldBe ByteVector(0xc0)
    BitVector.high(3).toByteVector shouldBe ByteVector(0xe0)
    BitVector.high(4).toByteVector shouldBe ByteVector(0xf0)
    BitVector.high(5).toByteVector shouldBe ByteVector(0xf8)
    BitVector.high(6).toByteVector shouldBe ByteVector(0xfc)
    BitVector.high(7).toByteVector shouldBe ByteVector(0xfe)
    BitVector.high(8).toByteVector shouldBe ByteVector(0xff)
    BitVector.high(9).toByteVector shouldBe ByteVector(0xff, 0x80)
    BitVector.high(10).toByteVector shouldBe ByteVector(0xff, 0xc0)
  }

  test("empty toByteVector") {
    BitVector.empty.toByteVector shouldBe ByteVector.empty
  }

  test("apply") {
    val vec = BitVector(ByteVector(0xf0, 0x0f))
    vec(0) should be (true)
    vec(1) should be (true)
    vec(2) should be (true)
    vec(3) should be (true)
    vec(4) should be (false)
    vec(5) should be (false)
    vec(6) should be (false)
    vec(7) should be (false)
    vec(8) should be (false)
    vec(9) should be (false)
    vec(10) should be (false)
    vec(11) should be (false)
    vec(12) should be (true)
    vec(13) should be (true)
    vec(14) should be (true)
    vec(15) should be (true)
  }

  test("updated") {
    val vec = BitVector.low(16)
    vec.set(6).get(6) should be (true)
    vec.set(10).get(10) should be (true)
    vec.set(10).clear(10).get(10) should be (false)
  }

  test("drop") {
    BitVector.high(8).drop(4).toByteVector shouldBe ByteVector(0xf0)
    BitVector.high(8).drop(3).toByteVector shouldBe ByteVector(0xf8)
    BitVector.high(10).drop(3).toByteVector shouldBe ByteVector(0xfe)
    BitVector.high(10).drop(3) shouldBe BitVector.high(7)
    BitVector.high(12).drop(3).toByteVector shouldBe ByteVector(0xff, 0x80)
    BitVector.empty.drop(4) shouldBe BitVector.empty
    BitVector.high(4).drop(8) shouldBe BitVector.empty
    forAll { (x: BitVector, n: Long) =>
      val m = if (x.nonEmpty) (n % x.size).abs else 0
      x.compact.drop(m).toIndexedSeq.take(4) shouldBe x.toIndexedSeq.drop(m.toInt).take(4)
      x.compact.drop(m).compact.toIndexedSeq.take(4) shouldBe x.toIndexedSeq.drop(m.toInt).take(4)
    }
  }

  test("take/drop") {
    BitVector.high(8).take(4).toByteVector shouldBe ByteVector(0xf0)
    BitVector.high(8).take(4) shouldBe BitVector.high(4)
    BitVector.high(8).take(5).toByteVector shouldBe ByteVector(0xf8)
    BitVector.high(8).take(5) shouldBe BitVector.high(5)
    BitVector.high(10).take(7).toByteVector shouldBe ByteVector(0xfe)
    BitVector.high(10).take(7) shouldBe BitVector.high(7)
    BitVector.high(12).take(9).toByteVector shouldBe ByteVector(0xff, 0x80)
    BitVector.high(12).take(9) shouldBe BitVector.high(9)
    BitVector.high(4).take(100).toByteVector shouldBe ByteVector(0xf0)
    forAll { (x: BitVector, n0: Long, m0: Long) =>
      (x.depth < 18) shouldBe true
      val m = if (x.nonEmpty) (m0 % x.size).abs else 0
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0
      (x.take(m) ++ x.drop(m)).compact shouldBe x
      x.take(m+n).compact.take(n) shouldBe x.take(n)
      x.drop(m+n).compact shouldBe x.drop(m).compact.drop(n)
      x.drop(n).take(m) shouldBe x.drop(n).take(m)
      x.drop(n).take(m).toIndexedSeq shouldBe BitVector.bits(x.drop(n).toIndexedSeq).take(m).toIndexedSeq
    }
  }

  test("dropRight") {
    BitVector.high(12).clear(0).dropRight(4).toByteVector shouldBe ByteVector(0x7f)
    forAll { (x: BitVector, n0: Long, m0: Long) =>
      val m = if (x.nonEmpty) (m0 % x.size).abs else 0
      val n =  if (x.nonEmpty) (n0 % x.size).abs else 0
      x.dropRight(m).dropRight(n) shouldBe x.dropRight(m + n)
      x.dropRight(m) shouldBe x.take(x.size - m)
    }
  }

  test("takeRight") {
    BitVector.high(12).clear(0).takeRight(4).toByteVector shouldBe ByteVector(0xf0)
    forAll { (x: BitVector, n0: Long, m0: Long) =>
      val m = if (x.nonEmpty) (m0 % x.size).abs else 0
      val n =  if (x.nonEmpty) (n0 % x.size).abs else 0
      x.takeRight(m max n).takeRight(n).compact shouldBe x.takeRight(n)
      x.takeRight(m) shouldBe x.drop(x.size - m)
    }
  }

  test("compact") {
    forAll { (x: BitVector) =>
      x.compact shouldBe x
      (x.force.depth < 18) shouldBe true
    }
  }

  test("++") {
    (BitVector.low(7) ++ BitVector.high(1)).toByteVector shouldBe ByteVector(1: Byte)
    (BitVector.high(8) ++ BitVector.high(8)).toByteVector shouldBe ByteVector(-1: Byte, -1: Byte)
    (BitVector.high(4) ++ BitVector.low(4)).toByteVector shouldBe ByteVector(0xf0)
    (BitVector.high(4) ++ BitVector.high(4)).toByteVector shouldBe ByteVector(-1: Byte)
    (BitVector.high(4) ++ BitVector.high(5)).toByteVector shouldBe ByteVector(-1: Byte, 0x80)
    (BitVector.low(2) ++ BitVector.high(4)).toByteVector shouldBe ByteVector(0x3c)
    (BitVector.low(2) ++ BitVector.high(4) ++ BitVector.low(2)).toByteVector shouldBe ByteVector(0x3c)
    forAll { (x: BitVector, y: BitVector) =>
      (x ++ y).compact.toIndexedSeq shouldBe (x.toIndexedSeq ++ y.toIndexedSeq)
    }
  }

  test("b.take(n).drop(n) == b") {
    implicit val intGen = Arbitrary(Gen.choose(0,10000))
    forAll { (xs: List[Boolean], n0: Int, m0: Int) =>
      whenever(xs.nonEmpty) {
        val n = n0.abs % xs.size
        val m = m0.abs % xs.size
        xs.drop(m).take(n) shouldBe xs.take(m+n).drop(m)
      }
    }
    forAll { (xs: BitVector, n0: Long) =>
      val m = if (xs.nonEmpty) n0 % xs.size else 0
      (xs.take(m) ++ xs.drop(m)).compact shouldBe xs
    }
  }

  test("<<") {
    (BitVector.high(8) << 0).toByteVector shouldBe ByteVector(0xff)
    (BitVector.high(8) << 4).toByteVector shouldBe ByteVector(0xf0)
    (BitVector.high(10) << 1).toByteVector shouldBe ByteVector(0xff, 0x80)
    (BitVector.high(10) << 3).toByteVector shouldBe ByteVector(0xfe, 0x00)
    (BitVector.high(32) << 16).toByteVector shouldBe ByteVector(0xff, 0xff, 0, 0)
    (BitVector.high(32) << 15).toByteVector shouldBe ByteVector(0xff, 0xff, 0x80, 0)
  }

  test(">>") {
    (BitVector.high(8) >> 8) shouldBe BitVector.high(8)
  }

  test(">>>") {
    (BitVector.high(8) >>> 7).toByteVector shouldBe ByteVector(0x01)
  }

  test("rotations") {
    bin"10101".rotateRight(3) shouldBe bin"10110"
    bin"10101".rotateLeft(3) shouldBe bin"01101"
    forAll { (b: BitVector, n: Long) =>
      b.rotateLeft(b.size) shouldBe b
      b.rotateRight(b.size) shouldBe b
      val n0 = if (b.nonEmpty) n % b.size else n
      b.rotateRight(n0).rotateLeft(n0) shouldBe b
      b.rotateLeft(n0).rotateRight(n0) shouldBe b
    }
  }

  test("padTo") {
    BitVector.high(2).padTo(8).toByteVector shouldBe ByteVector(0xc0)
    BitVector.high(16).padTo(32).toByteVector shouldBe ByteVector(0xff, 0xff, 0, 0)
  }

  test("~") {
    ~BitVector.high(12) shouldBe BitVector.low(12)
    ~BitVector.low(12) shouldBe BitVector.high(12)
    ~BitVector(10, 10) shouldBe BitVector(245, 245)
    ~BitVector(245, 245) shouldBe BitVector(10, 10)
  }

  test("&") {
    BitVector.high(16) & BitVector.high(16) shouldBe BitVector.high(16)
    BitVector.low(16) & BitVector.high(16) shouldBe BitVector.low(16)
    BitVector.high(16) & BitVector.low(16) shouldBe BitVector.low(16)
    BitVector.low(16) & BitVector.low(16) shouldBe BitVector.low(16)
    BitVector.high(16) & BitVector.high(9) shouldBe BitVector.high(9)
  }

  test("|") {
    BitVector.high(16) | BitVector.high(16) shouldBe BitVector.high(16)
    BitVector.low(16) | BitVector.high(16) shouldBe BitVector.high(16)
    BitVector.high(16) | BitVector.low(16) shouldBe BitVector.high(16)
    BitVector.low(16) | BitVector.low(16) shouldBe BitVector.low(16)
    BitVector.high(16) | BitVector.low(9) shouldBe BitVector.high(9)
  }

  test("^") {
    BitVector.high(16) ^ BitVector.high(16) shouldBe BitVector.low(16)
    BitVector.low(16) ^ BitVector.high(16) shouldBe BitVector.high(16)
    BitVector.high(16) ^ BitVector.low(16) shouldBe BitVector.high(16)
    BitVector.low(16) ^ BitVector.low(16) shouldBe BitVector.low(16)
    BitVector.high(16) ^ BitVector.low(9) shouldBe BitVector.high(9)
    BitVector(10, 245) ^ BitVector(245, 10) shouldBe BitVector.high(16)
  }

  test("toIndexedSeq") {
    BitVector.high(8).toIndexedSeq shouldBe List.fill(8)(true)
  }

  test("reverse") {
    BitVector(0x03).reverse shouldBe BitVector(0xc0)
    BitVector(0x03, 0x80).reverse shouldBe BitVector(0x01, 0xc0)
    BitVector(0x01, 0xc0).reverse shouldBe BitVector(0x03, 0x80)
    BitVector(0x30).take(4).reverse shouldBe BitVector(0xc0).take(4)
    forAll { (bv: BitVector) =>
      bv.reverse.reverse shouldBe bv
    }
  }

  test("reverseByteOrder") {
    BitVector(0x00, 0x01).reverseByteOrder shouldBe BitVector(0x01, 0x00)
    // Double reversing should yield original if size is divisible by 8
    forAll(genBitVector(500, 0)) { (bv: BitVector) =>
      bv.reverseByteOrder.reverseByteOrder shouldBe bv
    }
  }

  test("toHex") {
    BitVector(0x01, 0x02).toHex shouldBe "0102"
    BitVector(0x01, 0x02).drop(4).toHex shouldBe "102"
    BitVector(0x01, 0x02).drop(5).toHex shouldBe "204"
    forAll { (bv: BitVector) =>
      if (bv.size % 8 == 0 || bv.size % 8 > 4) bv.toHex shouldBe bv.toByteVector.toHex
      else bv.toHex shouldBe bv.toByteVector.toHex.init
    }
  }

  test("fromHexDescriptive") {
    BitVector.fromHexDescriptive("0x012") shouldBe Right(BitVector(0x01, 0x20).take(12))
    BitVector.fromHexDescriptive("0x01gg") shouldBe Left("Invalid hexadecimal character 'g' at index 4")
    forAll { (bv: BitVector) =>
      val x = bv.padTo((bv.size + 3) / 4 * 4)
      BitVector.fromValidHex(x.toHex) shouldBe x
    }
    BitVector.fromHexDescriptive("00 01 02 03") shouldBe Right(BitVector(0x00, 0x01, 0x02, 0x03))
  }

  test("fromValidHex") {
    BitVector.fromValidHex("0x012") shouldBe BitVector(0x01, 0x20).take(12)
    an[IllegalArgumentException] should be thrownBy { BitVector.fromValidHex("0x01gg") }
  }

  test("toBin") {
    BitVector(0x01, 0x02, 0xff).toBin shouldBe "000000010000001011111111"
    BitVector(0x01, 0x02, 0xff).drop(3).toBin shouldBe "000010000001011111111"
    forAll { (bv: BitVector) =>
      bv.toBin shouldBe bv.toByteVector.toBin.take(bv.size.toInt)
    }
  }

  test("fromBinDescriptive") {
    forAll { (bv: BitVector) =>
      BitVector.fromBinDescriptive(bv.toBin) shouldBe Right(bv)
    }
    BitVector.fromBinDescriptive("0102") shouldBe Left("Invalid binary character '2' at index 3")
    BitVector.fromBinDescriptive("0000 0001 0010 0011") shouldBe Right(BitVector(0x01, 0x23))
  }

  test("fromValidBin") {
    forAll { (bv: BitVector) =>
      BitVector.fromValidBin(bv.toBin) shouldBe bv
    }
    an[IllegalArgumentException] should be thrownBy { BitVector.fromValidBin("0x0102") }
  }

  test("bin string interpolator") {
    bin"0010" shouldBe BitVector(0x20).take(4)
    val x = BitVector.fromValidBin("10")
    bin"00$x" shouldBe BitVector(0x20).take(4)
    """bin"asdf"""" shouldNot compile
  }

  test("grouped + concatenate") {
    forAll { (bv: BitVector) =>
      if (bv.isEmpty) {
        bv.grouped(1) shouldBe Stream.empty
      } else if (bv.size < 3) {
        bv.grouped(bv.size) shouldBe Stream(bv)
      } else {
        bv.grouped(bv.size / 3).toList.foldLeft(BitVector.empty) { (acc, b) => acc ++ b } shouldBe bv
      }
    }
  }

  test("population count") {
    forAll { (bv: BitVector) =>
      val cnt = bv.toIndexedSeq.foldLeft(0) { (acc, b) => if (b) acc + 1 else acc }
      bv.populationCount shouldBe cnt
    }
  }

  test("indexOfSlice/containsSlice/startsWith") {
    forAll { (bv: BitVector, m0: Long, n0: Long) =>
      val m = if (bv.nonEmpty) (m0 % bv.size).abs else 0l
      val n = if (bv.nonEmpty) (n0 % bv.size).abs else 0l
      val slice = bv.slice(m min n, m max n)
      val idx = bv.indexOfSlice(slice)
      idx shouldBe bv.toIndexedSeq.indexOfSlice(slice.toIndexedSeq)
      bv.containsSlice(slice) shouldBe true
      if (bv.nonEmpty) bv.containsSlice(bv ++ bv) shouldBe false
    }
  }

  test("endsWith") {
    forAll { (bv: BitVector, n0: Long) =>
      val n = if (bv.nonEmpty) (n0 % bv.size).abs else 0l
      val slice = bv.takeRight(n)
      bv.endsWith(slice) shouldBe true
      if (slice.nonEmpty) bv.endsWith(~slice) shouldBe false
    }
  }

  test("splice") {
    forAll { (x: BitVector, y: BitVector, n0: Long) =>
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0l
      x.splice(n, BitVector.empty) shouldBe x
      x.splice(n, y) shouldBe (x.take(n) ++ y ++ x.drop(n))
    }
  }

  test("patch") {
    forAll { (x: BitVector, y: BitVector, n0: Long) =>
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0l
      x.patch(n, x.slice(n, n)) shouldBe x
      x.patch(n, y) shouldBe (x.take(n) ++ y ++ x.drop(n + y.size))
    }
  }

  test("sizeLessThan") { forAll { (x: BitVector) =>
    x.sizeLessThan(x.size+1) shouldBe true
    x.sizeLessThan(x.size) shouldBe false
  }}

  test("sizeGreaterThan") { forAll { (x: BitVector) =>
    (0 until x.size.toInt).forall(i => x.sizeGreaterThan(i)) shouldBe true
    x.sizeLessThan(x.size+1) shouldBe true
  }}

  test("sizeGreater/LessThan concurrent") { forAll { (x: BitVector) =>
    val ok = new java.util.concurrent.atomic.AtomicBoolean(true)
    def t = new Thread {
      override def start = {
        (0 until x.size.toInt).foreach { i =>
          ok.compareAndSet(true, x.sizeGreaterThan(i))
          ()
        }
      }
    }
    val t1 = t
    val t2 = t
    t1.start
    t2.start
    ok.compareAndSet(true, x.sizeLessThan(x.size+1))
    t1.join
    t2.join
    ok.get shouldBe true
  }}

  test("int conversions") {
    forAll { (n: Int) =>
      BitVector.fromInt(n).toInt() shouldBe n
      BitVector.fromInt(n, ordering = ByteOrdering.LittleEndian).toInt(ordering = ByteOrdering.LittleEndian) shouldBe n
    }
  }

  test("long conversions") {
    forAll { (n: Int) =>
      BitVector.fromLong(n).toLong() shouldBe n
      BitVector.fromLong(n, ordering = ByteOrdering.LittleEndian).toLong(ordering = ByteOrdering.LittleEndian) shouldBe n
    }
  }

  test("digest") {
    forAll { (x: BitVector) =>
      val sha256 = MessageDigest.getInstance("SHA-256")
      x.digest("SHA-256") shouldBe BitVector(ByteVector(sha256.digest(x.toByteArray)))
    }
  }

  test("serialization") {
    forAll { (x: BitVector) => serializationShouldRoundtrip(x) }
  }

  test("buffering") {
    implicit val longs = Arbitrary(Gen.choose(-1L,50L))

    def check(h: BitVector, xs: List[BitVector], delta: Long): Unit = {
      val unbuffered =
        BitVector.reduceBalanced(h :: xs)(_.size)(BitVector.Append(_,_))
      val buffered = xs.foldLeft(h)(_ ++ _)
      // sanity check for buffered
      (buffered.take(delta) ++ buffered.drop(delta)) shouldBe buffered
      // checks for consistency:
      buffered shouldBe unbuffered
      // get
      (0L until unbuffered.size).foreach { i =>
        buffered(i) shouldBe unbuffered(i)
      }
      // update
      val i = delta min (unbuffered.size - 1) max 0
      if (buffered.nonEmpty)
        buffered.update(i, i%2 == 0)(i) shouldBe unbuffered.update(i, i%2 == 0)(i)
      // size
      buffered.size shouldBe unbuffered.size
      // take
      buffered.take(delta) shouldBe unbuffered.take(delta)
      // drop
      buffered.drop(delta) shouldBe unbuffered.drop(delta)
    }

    forAll { (h: BitVector, xs: List[BitVector], delta: Long) =>
      check(h, xs, delta)
      // "evil" case for buffering - chunks of increasing sizes
      val evil = (h :: xs).sortBy(_.size)
      check(evil.head, evil.tail, delta)
    }
  }
}
