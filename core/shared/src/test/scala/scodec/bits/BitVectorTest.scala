/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec.bits

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

import java.util.UUID

import Arbitraries._

class BitVectorTest extends BitsSuite {
  implicit val arbitraryBitVector: Arbitrary[BitVector] = Arbitrary {
    Gen.oneOf(flatBytes, balancedTrees, splitVectors, concatSplitVectors, bitStreams)
  }

  property("hashCode/equals") {
    forAll { (b: BitVector, b2: BitVector, m: Long) =>
      assert((b.take(m) ++ b.drop(m)).hashCode == b.hashCode)
      if (b.take(3) == b2.take(3))
        // kind of weak, since this will only happen 1/8th of attempts on average
        assert(b.take(3).hashCode == b2.take(3).hashCode)
    }
  }

  property("=== consistent with ==") {
    forAll((b: BitVector, b2: BitVector) => assert((b == b2) == (b === b2)))
  }

  test("compact is a no-op for already compact bit vectors") {
    val b = BitVector(0x80, 0x90)
    assert((b.compact.underlying eq b.compact.underlying) == true)
    assert((b.toByteVector eq b.toByteVector) == true)
    val b2 =
      b.drop(8).align // also make sure this works if we're talking about a byte-aligned slice
    assert((b2.compact.underlying eq b2.compact.underlying) == true)
    assert((b2.toByteVector eq b2.toByteVector) == true)
  }

  property("equals/take/drop stack safety") {
    forAll(hugeBitStreams) { b =>
      assert(b == b) // this exercises take/drop
    }
  }

  property("hashCode/take/drop stack safety") {
    forAll(hugeBitStreams)(b => assert(b.hashCode == b.hashCode))
  }

  property("size stack safety") {
    forAll(hugeBitStreams)(b => assert(b.size == b.size))
  }

  test("acquire stack safety for lazy BitVector") {
    val nats = BitVector.unfold(0)(i => Some(BitVector.high(1000) -> (i + 1)))
    assert(nats.acquire(100000).isRight == true)
  }

  val bitVectorWithTakeIndex =
    Arbitrary.arbitrary[BitVector].flatMap(bits => Gen.choose(0L, bits.size + 1).map((bits, _)))

  property("acquire/take consistency") {
    def check(bits: BitVector, n: Long): Unit = {
      val b = bits.acquire(n)
      b match {
        case Left(_)   => assert(bits.size < n)
        case Right(hd) => assert(hd == bits.take(n))
      }
      assert(bits.acquireThen(n)(Left(_), Right(_)) == b)
      assert(bits.consumeThen(n)(Left(_), (a, b) => Right((b, a))) == bits.consume(n)(Right(_)))
      ()
    }

    forAll(bitVectorWithTakeIndex) { case (bits, ind) =>
      check(bits, ind)
      check(bits, ind * 2)
    }
  }

  test("1-bit vectors") {
    assert(BitVector.zero.head == false)
    assert(BitVector.one.head == true)
    assert(BitVector.bit(false).head == false)
    assert(BitVector.bit(true).head == true)
  }

  test("construction via high") {
    assert(BitVector.high(1).toByteVector == ByteVector(0x80))
    assert(BitVector.high(2).toByteVector == ByteVector(0xc0))
    assert(BitVector.high(3).toByteVector == ByteVector(0xe0))
    assert(BitVector.high(4).toByteVector == ByteVector(0xf0))
    assert(BitVector.high(5).toByteVector == ByteVector(0xf8))
    assert(BitVector.high(6).toByteVector == ByteVector(0xfc))
    assert(BitVector.high(7).toByteVector == ByteVector(0xfe))
    assert(BitVector.high(8).toByteVector == ByteVector(0xff))
    assert(BitVector.high(9).toByteVector == ByteVector(0xff, 0x80))
    assert(BitVector.high(10).toByteVector == ByteVector(0xff, 0xc0))
  }

  test("empty toByteVector") {
    assert(BitVector.empty.toByteVector == ByteVector.empty)
  }

  test("apply") {
    val vec = BitVector(ByteVector(0xf0, 0x0f))
    assert(vec(0))
    assert(vec(1))
    assert(vec(2))
    assert(vec(3))
    assert(!vec(4))
    assert(!vec(5))
    assert(!vec(6))
    assert(!vec(7))
    assert(!vec(8))
    assert(!vec(9))
    assert(!vec(10))
    assert(!vec(11))
    assert(vec(12))
    assert(vec(13))
    assert(vec(14))
    assert(vec(15))
  }

  property("getByte") {
    forAll { (x: BitVector) =>
      val bytes = x.bytes
      val aligned = x.align
      (0L until ((x.size + 7) / 8)).foreach { i =>
        assert(bytes(i) == x.getByte(i))
        assert(aligned.getByte(i) == x.getByte(i))
      }
    }
  }

  test("updated") {
    val vec = BitVector.low(16)
    assert(vec.set(6).get(6))
    assert(vec.set(10).get(10))
    assert(!vec.set(10).clear(10).get(10))
  }

  test("drop (1)") {
    assert(BitVector.high(8).drop(4).toByteVector == ByteVector(0xf0))
    assert(BitVector.high(8).drop(3).toByteVector == ByteVector(0xf8))
    assert(BitVector.high(10).drop(3).toByteVector == ByteVector(0xfe))
    assert(BitVector.high(10).drop(3) == BitVector.high(7))
    assert(BitVector.high(12).drop(3).toByteVector == ByteVector(0xff, 0x80))
    assert(BitVector.empty.drop(4) == BitVector.empty)
    assert(BitVector.high(4).drop(8) == BitVector.empty)
    assert(BitVector.high(8).drop(-20) == BitVector.high(8))
  }

  property("drop (2)") {
    forAll { (x: BitVector, n: Long) =>
      val m = if (x.nonEmpty) (n % x.size).abs else 0
      assert(x.compact.drop(m).toIndexedSeq.take(4) == x.toIndexedSeq.drop(m.toInt).take(4))
      assert(x.compact.drop(m).compact.toIndexedSeq.take(4) == x.toIndexedSeq.drop(m.toInt).take(4))
    }
  }

  test("take/drop (1)") {
    assert(BitVector.high(8).take(4).toByteVector == ByteVector(0xf0))
    assert(BitVector.high(8).take(4) == BitVector.high(4))
    assert(BitVector.high(8).take(5).toByteVector == ByteVector(0xf8))
    assert(BitVector.high(8).take(5) == BitVector.high(5))
    assert(BitVector.high(10).take(7).toByteVector == ByteVector(0xfe))
    assert(BitVector.high(10).take(7) == BitVector.high(7))
    assert(BitVector.high(12).take(9).toByteVector == ByteVector(0xff, 0x80))
    assert(BitVector.high(12).take(9) == BitVector.high(9))
    assert(BitVector.high(4).take(100).toByteVector == ByteVector(0xf0))
    assert(BitVector.high(12).take(-100) == BitVector.empty)
  }

  property("take/drop (2)") {
    forAll { (x: BitVector, n0: Long, m0: Long) =>
      assert(x.depth <= 18)
      val m = if (x.nonEmpty) (m0 % x.size).abs else 0
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0
      assert((x.take(m) ++ x.drop(m)).compact == x)
      assert(x.take(m + n).compact.take(n) == x.take(n))
      assert(x.drop(m + n).compact == x.drop(m).compact.drop(n))
      assert(x.drop(n).take(m) == x.drop(n).take(m))
      assert(
        x.drop(n).take(m).toIndexedSeq == BitVector
          .bits(x.drop(n).toIndexedSeq)
          .take(m)
          .toIndexedSeq
      )
    }
  }

  test("dropRight (1)") {
    assert(BitVector.high(12).clear(0).dropRight(4).toByteVector == ByteVector(0x7f))
  }

  property("dropRight (1)") {
    forAll { (x: BitVector, n0: Long, m0: Long) =>
      val m = if (x.nonEmpty) (m0 % x.size).abs else 0
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0
      assert(x.dropRight(m).dropRight(n) == x.dropRight(m + n))
      assert(x.dropRight(m) == x.take(x.size - m))
    }
  }

  test("takeRight (1)") {
    assert(BitVector.high(12).clear(0).takeRight(4).toByteVector == ByteVector(0xf0))
  }

  property("takeRight (2)") {
    forAll { (x: BitVector, n0: Long, m0: Long) =>
      val m = if (x.nonEmpty) (m0 % x.size).abs else 0
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0
      assert(x.takeRight(m.max(n)).takeRight(n).compact == x.takeRight(n))
      assert(x.takeRight(m) == x.drop(x.size - m))
    }
  }

  property("compact") {
    forAll { (x: BitVector) =>
      assert(x.compact == x)
      assert(x.force.depth < 36)
    }
  }

  test("depth") {
    // check that building a million byte vector via repeated snocs leads to balanced tree
    val N = 1000000
    val bits = (0 until N).map(n => BitVector(n)).foldLeft(BitVector.empty)(_ ++ _)
    assert(bits.depth < 36)
  }

  test("++ (1)") {
    assert((BitVector.low(7) ++ BitVector.high(1)).toByteVector == ByteVector(1: Byte))
    assert((BitVector.high(8) ++ BitVector.high(8)).toByteVector == ByteVector(-1: Byte, -1: Byte))
    assert((BitVector.high(4) ++ BitVector.low(4)).toByteVector == ByteVector(0xf0))
    assert((BitVector.high(4) ++ BitVector.high(4)).toByteVector == ByteVector(-1: Byte))
    assert(
      (BitVector.high(4) ++ BitVector.high(5)).toByteVector == ByteVector(-1.toByte.toInt, 0x80)
    )
    assert((BitVector.low(2) ++ BitVector.high(4)).toByteVector == ByteVector(0x3c))
    assert(
      (BitVector.low(2) ++ BitVector.high(4) ++ BitVector.low(2)).toByteVector == ByteVector(0x3c)
    )
  }

  property("++ (2)") {
    forAll { (x: BitVector, y: BitVector) =>
      assert((x ++ y).compact.toIndexedSeq == (x.toIndexedSeq ++ y.toIndexedSeq))
    }
  }

  property("b.take(n).drop(n) == b (1)") {
    forAll(
      Arbitrary.arbitrary[List[Boolean]],
      Gen.choose[Int](0, 10000),
      Gen.choose[Int](0, 10000)
    ) { (xs: List[Boolean], n0: Int, m0: Int) =>
      if (xs.nonEmpty) {
        val n = n0.abs % xs.size
        val m = m0.abs % xs.size
        assert(xs.drop(m).take(n) == xs.take(m + n).drop(m))
      }
    }
  }

  property("b.take(n).drop(n) == b (2)") {
    forAll { (xs: BitVector, n0: Long) =>
      val m = if (xs.nonEmpty) n0 % xs.size else 0
      assert((xs.take(m) ++ xs.drop(m)).compact == xs)
    }
  }

  test("<<") {
    assert((BitVector.high(8) << 0).toByteVector == ByteVector(0xff))
    assert((BitVector.high(8) << 4).toByteVector == ByteVector(0xf0))
    assert((BitVector.high(10) << 1).toByteVector == ByteVector(0xff, 0x80))
    assert((BitVector.high(10) << 3).toByteVector == ByteVector(0xfe, 0x00))
    assert((BitVector.high(32) << 16).toByteVector == ByteVector(0xff, 0xff, 0, 0))
    assert((BitVector.high(32) << 15).toByteVector == ByteVector(0xff, 0xff, 0x80, 0))
  }

  test(">>") {
    assert((BitVector.high(8) >> 8) == BitVector.high(8))
  }

  test(">>>") {
    assert((BitVector.high(8) >>> 7).toByteVector == ByteVector(0x01))
  }

  test("rotations (1)") {
    assert(bin"10101".rotateRight(3) == bin"10110")
    assert(bin"10101".rotateLeft(3) == bin"01101")
  }

  property("rotations (1)") {
    forAll { (b: BitVector, n: Long) =>
      assert(b.rotateLeft(b.size) == b)
      assert(b.rotateRight(b.size) == b)
      val n0 = if (b.nonEmpty) n % b.size else n
      assert(b.rotateRight(n0).rotateLeft(n0) == b)
      assert(b.rotateLeft(n0).rotateRight(n0) == b)
    }
  }

  test("padTo") {
    assert(BitVector.high(2).padTo(8).toByteVector == ByteVector(0xc0))
    assert(BitVector.high(16).padTo(32).toByteVector == ByteVector(0xff, 0xff, 0, 0))
  }

  test("~") {
    assert(~BitVector.high(12) == BitVector.low(12))
    assert(~BitVector.low(12) == BitVector.high(12))
    assert(~BitVector(10, 10) == BitVector(245, 245))
    assert(~BitVector(245, 245) == BitVector(10, 10))
  }

  test("&") {
    assert((BitVector.high(16) & BitVector.high(16)) == BitVector.high(16))
    assert((BitVector.low(16) & BitVector.high(16)) == BitVector.low(16))
    assert((BitVector.high(16) & BitVector.low(16)) == BitVector.low(16))
    assert((BitVector.low(16) & BitVector.low(16)) == BitVector.low(16))
    assert((BitVector.high(16) & BitVector.high(9)) == BitVector.high(9))
  }

  test("|") {
    assert((BitVector.high(16) | BitVector.high(16)) == BitVector.high(16))
    assert((BitVector.low(16) | BitVector.high(16)) == BitVector.high(16))
    assert((BitVector.high(16) | BitVector.low(16)) == BitVector.high(16))
    assert((BitVector.low(16) | BitVector.low(16)) == BitVector.low(16))
    assert((BitVector.high(16) | BitVector.low(9)) == BitVector.high(9))
  }

  test("^") {
    assert((BitVector.high(16) ^ BitVector.high(16)) == BitVector.low(16))
    assert((BitVector.low(16) ^ BitVector.high(16)) == BitVector.high(16))
    assert((BitVector.high(16) ^ BitVector.low(16)) == BitVector.high(16))
    assert((BitVector.low(16) ^ BitVector.low(16)) == BitVector.low(16))
    assert((BitVector.high(16) ^ BitVector.low(9)) == BitVector.high(9))
    assert((BitVector(10, 245) ^ BitVector(245, 10)) == BitVector.high(16))
  }

  test("toIndexedSeq") {
    assert(BitVector.high(8).toIndexedSeq == List.fill(8)(true))
  }

  test("reverse (1)") {
    assert(BitVector(0x03).reverse == BitVector(0xc0))
    assert(BitVector(0x03, 0x80).reverse == BitVector(0x01, 0xc0))
    assert(BitVector(0x01, 0xc0).reverse == BitVector(0x03, 0x80))
    assert(BitVector(0x30).take(4).reverse == BitVector(0xc0).take(4))
  }

  property("reverse (2)") {
    forAll((bv: BitVector) => assert(bv.reverse.reverse == bv))
  }

  test("reverseByteOrder (1)") {
    assert(BitVector(0x00, 0x01).reverseByteOrder == BitVector(0x01, 0x00))
  }

  property("reverseByteOrder (2)") {
    // Double reversing should yield original if size is divisible by 8
    forAll(genBitVector(500, 0)) { (bv: BitVector) =>
      assert(bv.reverseByteOrder.reverseByteOrder == bv)
    }
  }

  property("reverseByteOrder (3)") {
    forAll((bv: BitVector) => assert(bv.reverseByteOrder.invertReverseByteOrder == bv))
  }

  test("toHex (1)") {
    assert(BitVector(0x01, 0x02).toHex == "0102")
    assert(BitVector(0x01, 0x02).drop(4).toHex == "102")
    assert(BitVector(0x01, 0x02).drop(5).toHex == "204")
  }

  property("toHex (2)") {
    forAll { (bv: BitVector) =>
      if (bv.size % 8 == 0 || bv.size % 8 > 4) assert(bv.toHex == bv.toByteVector.toHex)
      else assert(bv.toHex == bv.toByteVector.toHex.init)
    }
  }

  test("fromHexDescriptive (1)") {
    assert(BitVector.fromHexDescriptive("0x012") == Right(BitVector(0x01, 0x20).take(12)))
    assert(
      BitVector.fromHexDescriptive("0x01gg") == Left("Invalid hexadecimal character 'g' at index 4")
    )
    assert(BitVector.fromHexDescriptive("00 01 02 03") == Right(BitVector(0x00, 0x01, 0x02, 0x03)))
  }

  property("fromHexDescriptive (2)") {
    forAll { (bv: BitVector) =>
      val x = bv.padTo((bv.size + 3) / 4 * 4)
      assert(BitVector.fromValidHex(x.toHex) == x)
    }
  }

  test("fromValidHex") {
    assert(BitVector.fromValidHex("0x012") == BitVector(0x01, 0x20).take(12))
    intercept[IllegalArgumentException](BitVector.fromValidHex("0x01gg"))
  }

  test("toBin (1)") {
    assert(BitVector(0x01, 0x02, 0xff).toBin == "000000010000001011111111")
    assert(BitVector(0x01, 0x02, 0xff).drop(3).toBin == "000010000001011111111")
  }

  property("toBin (2)") {
    forAll((bv: BitVector) => assert(bv.toBin == bv.toByteVector.toBin.take(bv.size.toInt)))
  }

  property("fromBinDescriptive (1)") {
    forAll((bv: BitVector) => assert(BitVector.fromBinDescriptive(bv.toBin) == Right(bv)))
  }

  test("fromBinDescriptive (2)") {
    assert(BitVector.fromBinDescriptive("0102") == Left("Invalid binary character '2' at index 3"))
    assert(BitVector.fromBinDescriptive("0000 0001 0010 0011") == Right(BitVector(0x01, 0x23)))
  }

  property("fromValidBin (1)") {
    forAll((bv: BitVector) => assert(BitVector.fromValidBin(bv.toBin) == bv))
  }

  test("fromValidBin (2)") {
    intercept[IllegalArgumentException](BitVector.fromValidBin("0x0102"))
  }

  test("bin string interpolator") {
    assert(bin"0010" == BitVector(0x20).take(4))
    compileErrors("""bin"asdf"""")
  }

  property("grouped + concatenate") {
    forAll { (bv: BitVector) =>
      if (bv.isEmpty)
        assert(bv.grouped(1).toList == Nil)
      else if (bv.size < 3)
        assert(bv.grouped(bv.size).toList == List(bv))
      else
        assert(bv.grouped(bv.size / 3).toList.foldLeft(BitVector.empty) { (acc, b) =>
          acc ++ b
        } == bv)
    }
  }

  property("population count") {
    forAll { (bv: BitVector) =>
      val cnt = bv.toIndexedSeq.foldLeft(0)((acc, b) => if (b) acc + 1 else acc)
      assert(bv.populationCount == cnt)
    }
  }

  property("indexOfSlice/containsSlice/startsWith") {
    forAll { (bv: BitVector, m0: Long, n0: Long) =>
      val m = if (bv.nonEmpty) (m0 % bv.size).abs else 0L
      val n = if (bv.nonEmpty) (n0 % bv.size).abs else 0L
      val slice = bv.slice(m.min(n), m.max(n))
      val idx = bv.indexOfSlice(slice)
      assert(idx == bv.toIndexedSeq.indexOfSlice(slice.toIndexedSeq))
      assert(bv.containsSlice(slice) == true)
      if (bv.nonEmpty) assert(bv.containsSlice(bv ++ bv) == false)
    }
  }

  property("endsWith") {
    forAll { (bv: BitVector, n0: Long) =>
      val n = if (bv.nonEmpty) (n0 % bv.size).abs else 0L
      val slice = bv.takeRight(n)
      assert(bv.endsWith(slice) == true)
      if (slice.nonEmpty) assert(bv.endsWith(~slice) == false)
    }
  }

  property("splice") {
    forAll { (x: BitVector, y: BitVector, n0: Long) =>
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0L
      assert(x.splice(n, BitVector.empty) == x)
      assert(x.splice(n, y) == (x.take(n) ++ y ++ x.drop(n)))
    }
  }

  property("patch") {
    forAll { (x: BitVector, y: BitVector, n0: Long) =>
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0L
      assert(x.patch(n, x.slice(n, n)) == x)
      assert(x.patch(n, y) == (x.take(n) ++ y ++ x.drop(n + y.size)))
    }
  }

  property("sizeLessThan") {
    forAll { (x: BitVector) =>
      assert(x.sizeLessThan(x.size + 1) == true)
      assert(x.sizeLessThan(x.size) == false)
    }
  }

  property("sizeGreaterThan") {
    forAll { (x: BitVector) =>
      assert((0 until x.size.toInt).forall(i => x.sizeGreaterThan(i.toLong)) == true)
      assert(x.sizeLessThan(x.size + 1) == true)
    }
  }

  property("byte conversions (1)") {
    forAll { (n: Byte) =>
      assert(BitVector.fromByte(n).toByte() == n)
      assert(BitVector.fromByte(n).sliceToShort(0, 8) == n)
      assert(BitVector.fromByte(n).sliceToShort(4, 4) == BitVector.fromByte(n).drop(4).toByte())
    }
  }

  test("byte conversions (2)") {
    assert(bin"11".toByte() == -1)
    assert(bin"11".toByte(signed = false) == 3)
    assert(BitVector.fromByte(3, 3) == bin"011")
  }

  property("short conversions (1)") {
    forAll { (n: Short) =>
      assert(BitVector.fromShort(n).toShort() == n)
      assert(
        BitVector
          .fromShort(n, ordering = ByteOrdering.LittleEndian)
          .toShort(ordering = ByteOrdering.LittleEndian) == n
      )
      assert(BitVector.fromShort(n).sliceToShort(0, 16) == n)
      assert(BitVector.fromShort(n).sliceToShort(4, 12) == BitVector.fromShort(n).drop(4).toShort())
      assert(
        BitVector.fromShort(n).sliceToShort(4, 12, ordering = ByteOrdering.LittleEndian) ==
          BitVector.fromShort(n).drop(4).toShort(ordering = ByteOrdering.LittleEndian)
      )
      if (n >= 0 && n < 16384) {
        assert(
          BitVector
            .fromShort(n, size = 15, ordering = ByteOrdering.BigEndian)
            .toShort(ordering = ByteOrdering.BigEndian) == n
        )
        assert(
          BitVector
            .fromShort(n, size = 15, ordering = ByteOrdering.LittleEndian)
            .toShort(ordering = ByteOrdering.LittleEndian) == n
        )
      }
    }
  }

  test("short conversions (2)") {
    assert(bin"11".toShort() == -1)
    assert(bin"11".toShort(signed = false) == 3)
  }

  test("int conversions") {
    forAll { (n: Int) =>
      assert(BitVector.fromInt(n).toInt() == n)
      assert(
        BitVector
          .fromInt(n, ordering = ByteOrdering.LittleEndian)
          .toInt(ordering = ByteOrdering.LittleEndian) == n
      )
      assert(BitVector.fromInt(n).sliceToInt(0, 32) == n)
      assert(BitVector.fromInt(n).sliceToInt(10, 22) == BitVector.fromInt(n).drop(10).toInt())
      assert(
        BitVector.fromInt(n).sliceToInt(10, 22, ordering = ByteOrdering.LittleEndian) ==
          BitVector.fromInt(n).drop(10).toInt(ordering = ByteOrdering.LittleEndian)
      )
      if (n >= -16383 && n < 16384) {
        assert(
          BitVector
            .fromInt(n, size = 15, ordering = ByteOrdering.BigEndian)
            .toInt(ordering = ByteOrdering.BigEndian) == n
        )
        assert(
          BitVector
            .fromInt(n, size = 15, ordering = ByteOrdering.LittleEndian)
            .toInt(ordering = ByteOrdering.LittleEndian) == n
        )
      }
    }
  }

  property("long conversions (1)") {
    forAll { (n: Long) =>
      assert(BitVector.fromLong(n).toLong() == n)
      assert(
        BitVector
          .fromLong(n, ordering = ByteOrdering.LittleEndian)
          .toLong(ordering = ByteOrdering.LittleEndian) == n
      )
      assert(BitVector.fromLong(n).sliceToLong(10, 54) == BitVector.fromLong(n).drop(10).toLong())
      assert(
        BitVector.fromLong(n).sliceToLong(10, 54, ordering = ByteOrdering.LittleEndian) ==
          BitVector.fromLong(n).drop(10).toLong(ordering = ByteOrdering.LittleEndian)
      )
      if (n >= -16383 && n < 16384) {
        assert(
          BitVector
            .fromLong(n, size = 15, ordering = ByteOrdering.BigEndian)
            .toLong(ordering = ByteOrdering.BigEndian) == n
        )
        assert(
          BitVector
            .fromLong(n, size = 15, ordering = ByteOrdering.LittleEndian)
            .toLong(ordering = ByteOrdering.LittleEndian) == n
        )
      }
    }
  }

  property("long conversions (2)") {
    forAll(Gen.choose(Long.MinValue >> 8, Long.MinValue >> 16)) { (n: Long) =>
      assert(
        BitVector
          .fromLong(n, size = 56, ordering = ByteOrdering.BigEndian)
          .toLong(ordering = ByteOrdering.BigEndian) == n
      )
      assert(
        BitVector
          .fromLong(n, size = 56, ordering = ByteOrdering.LittleEndian)
          .toLong(ordering = ByteOrdering.LittleEndian) == n
      )
    }
  }

  property("UUID conversions (1)") {
    // Valid conversions
    forAll((u: UUID) => assert(BitVector.fromUUID(u).toUUID == u))
  }

  property("UUID conversions (2)") {
    // "Invalid" conversions
    val badlySizedBitVector: Gen[BitVector] = arbitraryBitVector.arbitrary.suchThat(_.length != 128)
    forAll(badlySizedBitVector) { badlySizedBitVector =>
      intercept[IllegalArgumentException](badlySizedBitVector.toUUID)
      ()
    }
  }

  property("buffering") {
    implicit val longs: Arbitrary[Long] = Arbitrary(Gen.choose(-1L, 50L))

    def check(h: BitVector, xs: List[BitVector], delta: Long): Unit = {
      val unbuffered =
        BitVector.reduceBalanced(h :: xs)(_.size)(BitVector.Append(_, _))
      val buffered = xs.foldLeft(h)(_ ++ _)
      // sanity check for buffered
      assert((buffered.take(delta) ++ buffered.drop(delta)) == buffered)
      // checks for consistency:
      assert(buffered == unbuffered)
      // get
      (0L until unbuffered.size).foreach(i => assert(buffered(i) == unbuffered(i)))
      // update
      val i = delta.min(unbuffered.size - 1).max(0)
      if (buffered.nonEmpty)
        assert(buffered.update(i, i % 2 == 0)(i) == unbuffered.update(i, i % 2 == 0)(i))
      // size
      assert(buffered.size == unbuffered.size)
      // take
      assert(buffered.take(delta) == unbuffered.take(delta))
      // drop
      assert(buffered.drop(delta) == unbuffered.drop(delta))
      ()
    }

    forAll { (h: BitVector, xs: List[BitVector], delta: Long) =>
      check(h, xs, delta)
      // "evil" case for buffering - chunks of increasing sizes
      val evil = (h :: xs).sortBy(_.size)
      check(evil.head, evil.tail, delta)
    }
  }

  property("concat") {
    forAll { (bvs: List[BitVector]) =>
      val c = BitVector.concat(bvs)
      assert(c.size == bvs.map(_.size).foldLeft(0L)(_ + _))
      bvs.headOption.foreach(h => c.startsWith(h))
      bvs.lastOption.foreach(l => c.endsWith(l))
    }
  }

  test("slice") {
    assert(hex"001122334455".bits.slice(8, 32) == hex"112233".bits)
    assert(hex"001122334455".bits.slice(-21, 32) == hex"00112233".bits)
    assert(hex"001122334455".bits.slice(-21, -5) == hex"".bits)
  }

  property("sliceToByte") {
    forAll { (x: BitVector, offset0: Long, sliceSize0: Int) =>
      val offset = if (x.nonEmpty) (offset0 % x.size).abs else 0
      val sliceSize = (sliceSize0 % 9).abs.min((x.size - offset).toInt)
      assert(
        x.sliceToByte(offset, sliceSize).toInt == x.drop(offset).take(sliceSize.toLong).toInt()
      )
    }
  }

  test("highByte") {
    assert(BitVector.highByte.toBin == "11111111")
  }

  test("Ordered#compare") {
    assert(BitVector[Int]().compare(BitVector[Int]()) == 0)
    assert(BitVector(1).compare(BitVector(1)) == 0)
    assert(BitVector[Int]() < BitVector(1))
    assert(BitVector(1) < BitVector(1, 2))
    assert(BitVector(1) > BitVector[Int]())
    assert(BitVector(1, 2) > BitVector(1))
    assert(BitVector(1, 2) < BitVector(2))
    assert(BitVector(2) > BitVector(1, 2))
  }
}
