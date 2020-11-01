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

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.util.{Arrays, UUID}

import Arbitraries._
import org.scalacheck._
import Prop.forAll

class ByteVectorTest extends BitsSuite {

  property("hashCode/equals") {
    forAll(bytesWithIndex) { case (b, m) =>
      assert((b.take(m) ++ b.drop(m)) == b)
      assert((b.take(m) ++ b.drop(m)).hashCode == b.hashCode)
      if (b.take(3) == b.drop(3).take(3))
        // kind of weak, since this will only happen 1/8th of attempts on average
        assert(b.take(3).hashCode == b.drop(3).take(3).hashCode)
    }
  }

  test("issue #90") {
    val x = ByteVector.fill(Int.MaxValue.toLong + 1)(0)
    val y = ByteVector.fill(Int.MaxValue.toLong + 1)(1)
    assert((x === y) == false)
  }

  property("=== consistent with ==") {
    forAll((b: ByteVector, b2: ByteVector) => assert((b == b2) == (b === b2)))
  }

  test("compact is a no-op for already compact byte vectors") {
    val b = ByteVector(0x80)
    assert((b.compact eq b.compact) == true)
  }

  property("reverse.reverse == id") {
    forAll((b: ByteVector) => assert(b.reverse.reverse == b))
  }

  property("foldLeft") {
    forAll((b: ByteVector) => assert(b.foldLeft(ByteVector.empty)(_ :+ _) == b))
  }

  property("foldRight") {
    forAll((b: ByteVector) => assert(b.foldRight(ByteVector.empty)(_ +: _) == b))
  }

  test("insert (1)") {
    val b = ByteVector.empty
    assert(b.insert(0, 1) == ByteVector(1))
    assert(ByteVector(1, 2, 3, 4).insert(0, 0) == ByteVector(0, 1, 2, 3, 4))
    assert(ByteVector(1, 2, 3, 4).insert(1, 0) == ByteVector(1, 0, 2, 3, 4))
  }

  property("insert (2)") {
    forAll { (b: ByteVector) =>
      assert(b.foldLeft(ByteVector.empty)((acc, b) => acc.insert(acc.size, b)) == b)
    }
  }

  test("zipWith (1)") {
    val b1 = ByteVector(0, 1, 2, 3)
    val b2 = ByteVector(1, 2, 3, 4)
    assert(b1.zipWithI(b2)(_ + _) == ByteVector(1, 3, 5, 7))
  }

  property("zipWith (2)") {
    forAll((b: ByteVector) => assert(b.zipWithI(b)(_ - _) == ByteVector.fill(b.size)(0)))
  }

  test("zipWith2 (1)") {
    val b1 = ByteVector(0, 1, 2, 3)
    val b2 = ByteVector(1, 2, 3, 4)
    val b3 = ByteVector(2, 3, 4, 5)
    assert(b1.zipWithI2(b2, b3)(_ + _ + _) == ByteVector(3, 6, 9, 12))
  }

  property("zipWith2 (2)") {
    forAll((b: ByteVector) => assert(b.zipWithI2(b, b)(_ + _ - _) == b))
  }

  test("zipWith3 (1)") {
    val b1 = ByteVector(0, 1, 2, 3)
    val b2 = ByteVector(1, 2, 3, 4)
    val b3 = ByteVector(2, 3, 4, 5)
    val b4 = ByteVector(3, 4, 5, 6)
    assert(b1.zipWithI3(b2, b3, b4)(_ + _ + _ + _) == ByteVector(6, 10, 14, 18))
  }

  property("zipWith3 (2)") {
    forAll { (b: ByteVector) =>
      assert(b.zipWithI3(b, b, b)(_ + _ - _ - _) == ByteVector.fill(b.size)(0))
    }
  }

  property("consistent with Array[Byte] implementations (1)") {
    forAll(bytesWithIndex) { case (b, ind) =>
      val ba = b.toArray
      assert(Arrays.equals(b.take(ind).toArray, ba.take(ind.toInt)))
      assert(Arrays.equals(b.drop(ind).toArray, ba.drop(ind.toInt)))
      assert(b.lift(ind) == ba.lift(ind.toInt))
      assert(Arrays.equals(b.takeRight(ind).toArray, ba.takeRight(ind.toInt)))
      assert(Arrays.equals(b.dropRight(ind).toArray, ba.dropRight(ind.toInt)))
      assert(Arrays.equals(b.reverse.toArray, ba.reverse))
      assert(Arrays.equals(b.partialCompact(ind).toArray, ba))
      assert(b.lastOption == ba.lastOption)
      assert(b.nonEmpty == ba.nonEmpty)
      if (b.nonEmpty) {
        assert(b.last == ba.last)
        assert(Arrays.equals(b.init.toArray, ba.init))
      }
      if (ind < b.size) {
        val actual = b.update(ind, 9).toArray
        val correct = Vector(b.toIndexedSeq: _*).updated(ind.toInt, 9.toByte).toArray
        assert(Arrays.equals(actual, correct))
      }

    }
  }

  property("consistent with Array[Byte] implementations (2)") {
    forAll { (b1: ByteVector, b2: ByteVector) =>
      assert(Arrays.equals((b1 ++ b2).toArray, (b1.toArray ++ b2.toArray)))
    }
  }

  val deadbeef = ByteVector(0xde, 0xad, 0xbe, 0xef)

  test("toHex") {
    assert(deadbeef.toHex == "deadbeef")
  }

  test("fromHexDescriptive") {
    assert(ByteVector.fromHexDescriptive("0xdeadbeef") == Right(deadbeef))
    assert(ByteVector.fromHexDescriptive("0xDEADBEEF") == Right(deadbeef))
    assert(ByteVector.fromHexDescriptive("0XDEADBEEF") == Right(deadbeef))
    assert(ByteVector.fromHexDescriptive("deadbeef") == Right(deadbeef))
    assert(ByteVector.fromHexDescriptive("DEADBEEF") == Right(deadbeef))
    assert(ByteVector.fromHexDescriptive("de ad be ef") == Right(deadbeef))
    assert(ByteVector.fromHexDescriptive("de\tad\nbe\tef") == Right(deadbeef))
    assert(ByteVector.fromHexDescriptive("0xde_ad_be_ef") == Right(deadbeef))

    assert(ByteVector.fromHexDescriptive("0xdeadbee") == Right(ByteVector(0x0d, 0xea, 0xdb, 0xee)))
    assert(
      ByteVector.fromHexDescriptive("0xde_ad_be_e") == Right(ByteVector(0x0d, 0xea, 0xdb, 0xee))
    )

    assert(
      ByteVector
        .fromHexDescriptive("garbage") == Left("Invalid hexadecimal character 'g' at index 0")
    )
    assert(
      ByteVector
        .fromHexDescriptive("deadbefg") == Left("Invalid hexadecimal character 'g' at index 7")
    )
  }

  test("toBin") {
    assert(deadbeef.toBin == "11011110101011011011111011101111")
  }

  test("fromBinDescriptive") {
    assert(ByteVector.fromBinDescriptive(deadbeef.toBin) == Right(deadbeef))
    assert(
      ByteVector.fromBinDescriptive(deadbeef.toBin.grouped(4).mkString(" ")) == Right(deadbeef)
    )
    assert(ByteVector.fromBinDescriptive("0001 0011") == Right(ByteVector(0x13)))
    assert(ByteVector.fromBinDescriptive("0b 0001 0011 0111") == Right(ByteVector(0x01, 0x37)))
    assert(
      ByteVector.fromBinDescriptive("1101a000") == Left("Invalid binary character 'a' at index 4")
    )
    assert(
      ByteVector.fromBinDescriptive("0b1101a000") == Left("Invalid binary character 'a' at index 6")
    )
    assert(
      ByteVector.fromBinDescriptive("0B1101a000") == Left("Invalid binary character 'a' at index 6")
    )
  }

  test("fromValidBin") {
    assert(ByteVector.fromValidBin(deadbeef.toBin) == deadbeef)
    intercept[IllegalArgumentException](ByteVector.fromValidBin("1101a000"))
  }

  test("toBase32") {
    assert(hex"".toBase32 == "")
    assert(hex"00".toBase32 == "AA======")
    assert(hex"61".toBase32 == "ME======")
    assert(hex"626262".toBase32 == "MJRGE===")
    assert(hex"636363".toBase32 == "MNRWG===")
    assert(
      hex"73696d706c792061206c6f6e6720737472696e67".toBase32 == "ONUW24DMPEQGCIDMN5XGOIDTORZGS3TH"
    )
    assert(
      hex"00eb15231dfceb60925886b67d065299925915aeb172c06647".toBase32 == "ADVRKIY57TVWBESYQ23H2BSSTGJFSFNOWFZMAZSH"
    )
    assert(hex"516b6fcd0f".toBase32 == "KFVW7TIP")
    assert(hex"bf4f89001e670274dd".toBase32 == "X5HYSAA6M4BHJXI=")
    assert(hex"572e4794".toBase32 == "K4XEPFA=")
    assert(hex"ecac89cad93923c02321".toBase32 == "5SWITSWZHER4AIZB")
    assert(hex"10c8511e".toBase32 == "CDEFCHQ=")
    assert(hex"00000000000000000000".toBase32 == "AAAAAAAAAAAAAAAA")
  }

  test("fromValidBase32") {
    assert(ByteVector.fromValidBase32("") == (ByteVector.empty))
    assert(ByteVector.fromValidBase32("AA======") == hex"00")
    assert(ByteVector.fromValidBase32("ME======") == hex"61")
    assert(ByteVector.fromValidBase32("MJRGE===") == hex"626262")
    assert(ByteVector.fromValidBase32("MNRWG===") == hex"636363")
    assert(
      ByteVector
        .fromValidBase32(
          "ONUW24DMPEQGCIDMN5XGOIDTORZGS3TH"
        ) == hex"73696d706c792061206c6f6e6720737472696e67"
    )
    assert(
      ByteVector
        .fromValidBase32(
          "ADVRKIY57TVWBESYQ23H2BSSTGJFSFNOWFZMAZSH"
        ) == hex"00eb15231dfceb60925886b67d065299925915aeb172c06647"
    )
    assert(ByteVector.fromValidBase32("KFVW7TIP") == hex"516b6fcd0f")
    assert(ByteVector.fromValidBase32("X5HYSAA6M4BHJXI=") == hex"bf4f89001e670274dd")
    assert(ByteVector.fromValidBase32("K4XEPFA=") == hex"572e4794")
    assert(ByteVector.fromValidBase32("5SWITSWZHER4AIZB") == hex"ecac89cad93923c02321")
    assert(ByteVector.fromValidBase32("CDEFCHQ=") == hex"10c8511e")
    assert(ByteVector.fromValidBase32("AAAAAAAAAAAAAAAA") == hex"00000000000000000000")
  }

  test("fail due to illegal character fromBase32") {
    assert(
      ByteVector
        .fromBase32Descriptive("7654321") == Left("Invalid base 32 character '1' at index 6")
    )
    assert(
      ByteVector
        .fromBase32Descriptive("ABc") == Left("Invalid base 32 character 'c' at index 2")
    )
    assert(
      ByteVector
        .fromBase32Descriptive("AB CD 0") == Left("Invalid base 32 character '0' at index 6")
    )
    assert(ByteVector.fromBase32("a").isEmpty)
  }

  test("toBase32(Crockford)") {
    import scodec.bits.Bases.Alphabets.{Base32Crockford => Crockford}

    assert(hex"".toBase32(Crockford) == "")
    assert(hex"00".toBase32(Crockford) == "00======")
    assert(hex"ffffffffff".toBase32(Crockford) == "ZZZZZZZZ")
    assert(hex"8ca74adb5b".toBase32(Crockford) == "HJKMNPTV")
  }

  test("fromValidBase32(Crockford)") {
    import scodec.bits.Bases.Alphabets.{Base32Crockford => Crockford}

    assert(ByteVector.fromValidBase32("", Crockford) == (ByteVector.empty))
    assert(ByteVector.fromValidBase32("00======", Crockford) == hex"00")
    assert(ByteVector.fromValidBase32("ZZZZZZZZ", Crockford) == hex"ffffffffff")
    assert(ByteVector.fromValidBase32("HJKMNPTV", Crockford) == hex"8ca74adb5b")
    assert(ByteVector.fromValidBase32("hjkmnptv", Crockford) == hex"8ca74adb5b")
    assert(ByteVector.fromValidBase32("00011111", Crockford) == hex"0000108421")
    assert(ByteVector.fromValidBase32("0Oo1IiLl", Crockford) == hex"0000108421")
  }

  test("toBase58") {
    assert(hex"".toBase58 == "")
    assert(hex"00".toBase58 == "1")
    assert(hex"61".toBase58 == "2g")
    assert(hex"626262".toBase58 == "a3gV")
    assert(hex"636363".toBase58 == "aPEr")
    assert(
      hex"73696d706c792061206c6f6e6720737472696e67".toBase58 == "2cFupjhnEsSn59qHXstmK2ffpLv2"
    )
    assert(
      hex"00eb15231dfceb60925886b67d065299925915aeb172c06647".toBase58 == "1NS17iag9jJgTHD1VXjvLCEnZuQ3rJDE9L"
    )
    assert(hex"516b6fcd0f".toBase58 == "ABnLTmg")
    assert(hex"bf4f89001e670274dd".toBase58 == "3SEo3LWLoPntC")
    assert(hex"572e4794".toBase58 == "3EFU7m")
    assert(hex"ecac89cad93923c02321".toBase58 == "EJDM8drfXA6uyA")
    assert(hex"10c8511e".toBase58 == "Rt5zm")
    assert(hex"00000000000000000000".toBase58 == "1111111111")
  }

  test("fromValidBase58") {
    assert(ByteVector.fromValidBase58("") == (ByteVector.empty))
    assert(ByteVector.fromValidBase58("1") == hex"00")
    assert(ByteVector.fromValidBase58("2g") == hex"61")
    assert(ByteVector.fromValidBase58("a3gV") == hex"626262")
    assert(ByteVector.fromValidBase58("aPEr") == hex"636363")
    assert(
      ByteVector
        .fromValidBase58(
          "2cFupjhnEsSn59qHXstmK2ffpLv2"
        ) == hex"73696d706c792061206c6f6e6720737472696e67"
    )
    assert(
      ByteVector
        .fromValidBase58(
          "1NS17iag9jJgTHD1VXjvLCEnZuQ3rJDE9L"
        ) == hex"00eb15231dfceb60925886b67d065299925915aeb172c06647"
    )
    assert(ByteVector.fromValidBase58("ABnLTmg") == hex"516b6fcd0f")
    assert(ByteVector.fromValidBase58("3SEo3LWLoPntC") == hex"bf4f89001e670274dd")
    assert(ByteVector.fromValidBase58("3EFU7m") == hex"572e4794")
    assert(ByteVector.fromValidBase58("EJDM8drfXA6uyA") == hex"ecac89cad93923c02321")
    assert(ByteVector.fromValidBase58("Rt5zm") == hex"10c8511e")
    assert(ByteVector.fromValidBase58("1111111111") == hex"00000000000000000000")
  }

  test("fail due to illegal character fromBase58") {
    assert(
      ByteVector
        .fromBase58Descriptive("R3C0NFxN") == Left("Invalid base 58 character '0' at index 3")
    )
    assert(
      ByteVector
        .fromBase58Descriptive("03CMNFxN") == Left("Invalid base 58 character '0' at index 0")
    )
    assert(ByteVector.fromBase58("3CMNFxN1oHBc4R1EpboAL5yzHGgE611Xol").isEmpty)
  }

  property("base64 roundtrip") {
    forAll((b: ByteVector) => assert(ByteVector.fromValidBase64(b.toBase64) == b))
  }

  test("base64 issue #45") {
    val base64 =
      "1MOyoQIABAAAAAAAAAAAAP//AAABAAAAPl6hVQvgDAA8AAAAPAAAAP///////wAhQwjkUwgARQAA\r\nKEPjAABAEd9lqf4Bgan+Af/a/hOIABSGXENNRAAAAAAbqf4B/wAAAAAAAD9eoVX52QYAPAAAADwA\r\nAAABgMIAAAAAH5AHOpIAJkJCAwAAAAAAkAAADlgwS+AAAAA3kAAADlgwS+CAAgIABgABAAQAc2Vy\r\nYwAAAAA="
    assert(BitVector.fromBase64Descriptive(base64).map(_.size) == Right(1408))
  }

  property("buffer :+") {
    forAll { (b: ByteVector, bs: List[ByteVector], n: Int) =>
      val unbuf = bs.foldLeft(b)(_ ++ _)
      val buf = bs.foldLeft(b.bufferBy((n % 50).max(0) + 1))((acc, a) => a.foldLeft(acc)(_ :+ _))
      assert(unbuf == buf)
    }
  }

  property("buffer ++/take/drop") {
    forAll { (b: ByteVector, bs: List[ByteVector], n: Int) =>
      val unbuf = bs.foldLeft(b)(_ ++ _)
      val buf = bs.foldLeft(b.bufferBy((n % 50).max(0) + 1))(_ ++ _)
      assert(unbuf == buf)
      val ind = (n % (unbuf.size + 1)).max(0) + 1
      assert(buf.take(ind) == unbuf.take(ind))
      assert(buf.drop(ind) == unbuf.drop(ind))
    }
  }

  property("buffer rebuffering") {
    forAll { (b1: ByteVector, b2: ByteVector, b3: ByteVector, n: Int) =>
      val chunkSize = (n % 50).max(0) + 1
      val b1b = b1.bufferBy(chunkSize)
      val b1b2b3 = (b1b ++ b2).bufferBy(chunkSize + 1) ++ b3
      assert(b1b2b3 == (b1 ++ b2 ++ b3))
    }
  }

  test("<<") {
    assert(ByteVector(0x55, 0x55, 0x55) << 1 == ByteVector(0xaa, 0xaa, 0xaa))
  }

  test(">>") {
    assert(ByteVector(0x55, 0x55, 0x55) >> 1 == ByteVector(0x2a, 0xaa, 0xaa))
    assert(ByteVector(0xaa, 0xaa, 0xaa) >> 1 == ByteVector(0xd5, 0x55, 0x55))
  }

  test(">>>") {
    assert(ByteVector(0x55, 0x55, 0x55) >>> 1 == ByteVector(0x2a, 0xaa, 0xaa))
    assert(ByteVector(0xaa, 0xaa, 0xaa) >>> 1 == ByteVector(0x55, 0x55, 0x55))
  }

  property("rotations") {
    forAll { (b: ByteVector, n: Long) =>
      assert(b.rotateLeft(b.size * 8) == b)
      assert(b.rotateRight(b.size * 8) == b)
      assert(b.rotateRight(n).rotateLeft(n) == b)
      assert(b.rotateLeft(n).rotateRight(n) == b)
    }
  }

  test("hex string interpolator") {
    assert(hex"deadbeef" == deadbeef)
    compileErrors("""hex"deadgg"""")
  }

  property("toIterable roundtrip") {
    forAll { (b: ByteVector) =>
      val fromIter = ByteVector(b.toIterable)
      assert(b == fromIter)
      assert(fromIter == b)
    }
  }

  property("toArray roundtrip") {
    forAll { (b: ByteVector) =>
      val fromArr = ByteVector(b.toArray)
      assert(b == fromArr)
      assert(fromArr == b)
      // Ensure immutable behavior
      val fromArr2 = ByteVector(b.toArray)
      assert(fromArr == fromArr2)
    }
  }

  property("copyToStream roundtrip") {
    forAll { (b: ByteVector) =>
      val os = new ByteArrayOutputStream()
      b.copyToStream(os)
      val fromArr = ByteVector(os.toByteArray)
      assert(b == fromArr)
      assert(fromArr == b)
    }
  }

  property("toByteBuffer roundtrip") {
    forAll { (b: ByteVector) =>
      val fromBuffer = ByteVector(b.toByteBuffer)
      assert(b == fromBuffer)
      assert(fromBuffer == b)
    }
  }

  property("dropping from a view is consistent with dropping from a strict vector") {
    forAll { (b: ByteVector, n0: Long) =>
      val view = ByteVector.view(b.toArray)
      val n = n0.abs
      assert(b.drop(n) == view.drop(n))
    }
  }

  property("grouped + concatenate") {
    forAll { (bv: ByteVector) =>
      if (bv.isEmpty)
        assert(bv.grouped(1).toList == Nil)
      else if (bv.size < 3)
        assert(bv.grouped(bv.size).toList == List(bv))
      else
        assert(bv.grouped(bv.size / 3).toList.foldLeft(ByteVector.empty) { (acc, b) =>
          acc ++ b
        } == bv)
    }
  }

  property("indexOfSlice/containsSlice/startsWith") {
    forAll { (bv: ByteVector, m0: Int, n0: Int) =>
      val m = if (bv.nonEmpty) (m0 % bv.size).abs else 0
      val n = if (bv.nonEmpty) (n0 % bv.size).abs else 0
      val slice = bv.slice(m.min(n), m.max(n))
      val idx = bv.indexOfSlice(slice)
      assert(idx == bv.toIndexedSeq.indexOfSlice(slice.toIndexedSeq))
      assert(bv.containsSlice(slice) == true)
      if (bv.nonEmpty) assert(bv.containsSlice(bv ++ bv) == false)
    }
  }

  property("endsWith") {
    forAll { (bv: ByteVector, n0: Int) =>
      val n = if (bv.nonEmpty) (n0 % bv.size).abs else 0
      val slice = bv.takeRight(n)
      assert(bv.endsWith(slice) == true)
      if (slice.nonEmpty) assert(bv.endsWith(~slice) == false)
    }
  }

  property("splice") {
    forAll { (x: ByteVector, y: ByteVector, n0: Int) =>
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0
      assert(x.splice(n, ByteVector.empty) == x)
      assert(x.splice(n, y) == (x.take(n) ++ y ++ x.drop(n)))
    }
  }

  property("patch") {
    forAll { (x: ByteVector, y: ByteVector, n0: Int) =>
      val n = if (x.nonEmpty) (n0 % x.size).abs else 0
      assert(x.patch(n, x.slice(n, n)) == x)
      assert(x.patch(n, y) == (x.take(n) ++ y ++ x.drop(n + y.size)))
    }
  }

  property("short conversions") {
    forAll { (n: Short) =>
      assert(ByteVector.fromShort(n).toShort() == n)
      assert(
        ByteVector
          .fromShort(n, ordering = ByteOrdering.LittleEndian)
          .toShort(ordering = ByteOrdering.LittleEndian) == n
      )
    }
  }

  property("int conversions") {
    forAll { (n: Int) =>
      assert(ByteVector.fromInt(n).toInt() == n)
      assert(
        ByteVector
          .fromInt(n, ordering = ByteOrdering.LittleEndian)
          .toInt(ordering = ByteOrdering.LittleEndian) == n
      )
    }
  }

  property("long conversions") {
    forAll { (n: Long) =>
      assert(ByteVector.fromLong(n).toLong() == n)
      assert(
        ByteVector
          .fromLong(n, ordering = ByteOrdering.LittleEndian)
          .toLong(ordering = ByteOrdering.LittleEndian) == n
      )
    }
  }

  property("UUID conversions (1)") {
    // Valid conversions
    forAll((u: UUID) => assert(ByteVector.fromUUID(u).toUUID == u))
  }

  property("UUID conversions (2)") {
    // "Invalid" conversions
    val badlySizedByteVector: Gen[ByteVector] = byteVectors.suchThat(_.length != 16)
    forAll(badlySizedByteVector) { badlySizedByteVector =>
      intercept[IllegalArgumentException](badlySizedByteVector.toUUID)
      ()
    }
  }

  property("concat") {
    forAll { (bvs: List[ByteVector]) =>
      val c = ByteVector.concat(bvs)
      assert(c.size == bvs.map(_.size).foldLeft(0L)(_ + _))
      bvs.headOption.foreach(h => c.startsWith(h))
      bvs.lastOption.foreach(l => c.endsWith(l))
    }
  }

  property("copyToArray with offset/size") {
    forAll { (b: ByteVector) =>
      val size = b.size / 3
      val start = b.size / 4
      val offset = b.size / 5
      val xs = new Array[Byte](b.size.toInt)
      b.copyToArray(xs, start.toInt, offset, size.toInt)
      val startPlusSize = start + size
      assert(
        Arrays.equals(
          xs,
          (xs.take(start.toInt) ++ b.drop(offset).take(size).toArray ++ xs.drop(
            startPlusSize.toInt
          )).toArray
        )
      )
    }
  }

  property("copyToBuffer") {
    forAll { (b: ByteVector, bufferSize0: Int, initialPosition0: Int, direct: Boolean) =>
      val bufferSize = (bufferSize0 % 1000000).abs
      val buffer =
        if (direct) ByteBuffer.allocateDirect(bufferSize) else ByteBuffer.allocate(bufferSize)
      val initialPosition = if (bufferSize == 0) 0 else (initialPosition0 % bufferSize).abs
      buffer.position(initialPosition)
      val copied = b.copyToBuffer(buffer)
      buffer.flip()
      assert(copied == ((bufferSize.toLong - initialPosition).min(b.size)))
      assert(ByteVector.view(buffer).drop(initialPosition.toLong) == b.take(copied.toLong))
    }
  }

  property("viewing ByteBuffer with non-zero position") {
    forAll { (b: Array[Byte], position0: Int, sliceSize0: Int, direct: Boolean) =>
      val buffer = if (direct) ByteBuffer.allocateDirect(b.size) else ByteBuffer.allocate(b.size)
      val position = if (b.size == 0) 0 else (position0 % b.size).abs
      val remaining = b.size - position
      val sliceSize = if (remaining == 0) 0 else (sliceSize0 % (b.size - position)).abs

      buffer.position(position).limit(position + sliceSize)
      val slice = buffer.slice()
      buffer.position(position).limit(position + sliceSize)
      assert(ByteVector.view(buffer) == ByteVector.view(slice))

      buffer.position(position)
      ()
    }
  }

  property("dropWhile") {
    forAll { (x: ByteVector) =>
      val (expected, _) = x.foldLeft((ByteVector.empty, true)) { case ((acc, dropping), b) =>
        if (dropping)
          if (b == 0) (acc :+ 0, false)
          else (acc, true)
        else
          (acc :+ b, false)
      }
      assert(x.dropWhile(_ != 0.toByte) == expected)
    }
  }

  property("takeWhile") {
    forAll { (x: ByteVector) =>
      val (expected, _) = x.foldLeft((ByteVector.empty, true)) { case ((acc, taking), b) =>
        if (taking)
          if (b == 0) (acc, false)
          else (acc :+ b, true)
        else
          (acc, false)
      }
      assert(x.takeWhile(_ != 0.toByte) == expected)
    }
  }

  test("very large vectors") {
    val huge = ByteVector.fill(Int.MaxValue * 2L)(0)
    val huge2 = huge ++ huge ++ hex"deadbeef"
    assert(huge2.takeRight(2) == hex"beef")
  }

  test("take") {
    assert(hex"0011223344".take(3) == hex"001122")
    assert(hex"0011223344".take(1000) == hex"0011223344")
    assert(hex"0011223344".take(-10) == hex"")
  }

  test("drop") {
    assert(hex"0011223344".drop(3) == hex"3344")
    assert(hex"0011223344".drop(-10) == hex"0011223344")
    assert(hex"0011223344".drop(1000) == hex"")
  }

  test("slice") {
    assert(hex"001122334455".slice(1, 4) == hex"112233")
    assert(hex"001122334455".slice(-21, 4) == hex"00112233")
    assert(hex"001122334455".slice(-21, -4) == hex"")
  }

  property("slice is consistent with array slice") {
    forAll { (b: ByteVector, from: Int, until: Int) =>
      assert(b.slice(from.toLong, until.toLong) == ByteVector.view(b.toArray.slice(from, until)))
    }
  }

  test("unapply") {
    val ByteVector(x, y, z) = hex"000102"
    assert(x == 0.toByte)
    assert(y == 1.toByte)
    assert(z == 2.toByte)

    hex"000102" match {
      case ByteVector(0, 1, 2) => // OK
    }
  }

  test("Ordered#compare") {
    assert(ByteVector[Int]().compare(ByteVector[Int]()) == 0)
    assert(ByteVector(1).compare(ByteVector(1)) == 0)
    assert(ByteVector[Int]() < ByteVector(1))
    assert(ByteVector(1) < ByteVector(1, 2))
    assert(ByteVector(1) > ByteVector[Int]())
    assert(ByteVector(1, 2) > ByteVector(1))
    assert(ByteVector(1, 2) < ByteVector(2))
    assert(ByteVector(2) > ByteVector(1, 2))
    assert(ByteVector(100) < ByteVector(200))
  }
}
