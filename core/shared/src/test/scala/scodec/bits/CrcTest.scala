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

import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import Arbitraries._

class CrcTest extends ScalaCheckSuite {

  implicit val arbBitVector: Arbitrary[BitVector] = Arbitrary(genBitVector())

  def crcWith(
      poly: String,
      initial: String,
      reflectInput: Boolean,
      reflectOutput: Boolean,
      finalXor: String
  ) =
    crc(
      BitVector.fromValidHex(poly),
      BitVector.fromValidHex(initial),
      reflectInput,
      reflectOutput,
      BitVector.fromValidHex(finalXor)
    )

  property("table based implementation should have the same result as bitwise implementation") {
    val poly = hex"04C11DB7".bits
    val table32 = crc(poly, hex"ffffffff".bits, false, false, hex"00000000".bits)
    forAll { (bv: BitVector) =>
      assert(
        table32(bv) == crc.bitwise(
          poly,
          hex"ffffffff".bits,
          false,
          false,
          hex"00000000".bits,
          bv
        )
      )
    }
  }

  val checkBytes = BitVector("123456789".getBytes("US-ASCII"))
  def check(crc: BitVector => BitVector, expected: String) =
    assert(crc(checkBytes) == BitVector.fromValidHex(expected))

  test("CRC-3/ROHC") {
    val crc3rohc = crc(bin"011", bin"111", true, true, bin"000")
    assert(crc3rohc(checkBytes) == bin"110")
  }

  test("CRC-4/ITU") {
    val crc4itu = crc(bin"0011", bin"0000", true, true, bin"0000")
    assert(crc4itu(checkBytes) == bin"0111")
  }

  test("CRC-5/EPC") {
    val crc5epc = crc(bin"01001", bin"01001", false, false, bin"00000")
    assert(crc5epc(checkBytes) == bin"00000")
  }

  test("CRC-5/USB") {
    val crc5usb = crc(bin"00101", bin"11111", true, true, bin"11111")
    assert(crc5usb(checkBytes) == bin"11001")
  }

  test("CRC-5/ITU") {
    val crc5itu =
      crc(hex"09".bits.drop(1), hex"00".bits.drop(1), false, false, hex"00".bits.drop(1))
    assert(crc5itu(checkBytes) == hex"75".bits.drop(1))
  }

  test("CRC-7") {
    val crc7 = crc(hex"15".bits.drop(3), hex"00".bits.drop(3), true, true, hex"00".bits.drop(3))
    assert(crc7(checkBytes) == hex"07".bits.drop(3))
  }

  test("CCITT-16") {
    val ccitt16 = crcWith("1021", "ffff", false, false, "0000")
    assert(ccitt16(hex"12345670".bits) == hex"b1e4".bits)
    assert(ccitt16(hex"5a261977".bits) == hex"1aad".bits)
  }

  test("CRC-32") {
    val crc32 = crcWith("04c11db7", "ffffffff", true, true, "ffffffff")
    check(crc32, "cbf43926")
  }

  test("CRC-32c") {
    val crc32c = crcWith("1edc6f41", "ffffffff", true, true, "ffffffff")
    check(crc32c, "e3069283")
  }

  test("CRC-40/GSM") {
    val crc40gsm = crcWith("0004820009", "0000000000", false, false, "ffffffffff")
    check(crc40gsm, "d4164fc646")
  }

  test("CRC-82/DARC") {
    val crc82darc = crc(
      hex"0308c0111011401440411".bits.drop(6),
      hex"000000000000000000000".bits.drop(6),
      true,
      true,
      hex"000000000000000000000".bits.drop(6)
    )
    assert(crc82darc(checkBytes) == hex"09ea83f625023801fd612".bits.drop(6))
  }
}
