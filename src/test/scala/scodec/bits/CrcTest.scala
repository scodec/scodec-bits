package scodec.bits

import org.scalacheck.Arbitrary
import org.scalatest.{ FunSuite, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import Arbitraries._

class CrcTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {

  implicit val arbBitVector = Arbitrary(genBitVector())

  def crcWith(poly: String, initial: String, reflectInput: Boolean, reflectOutput: Boolean, finalXor: String) =
    crc(BitVector.fromValidHex(poly), BitVector.fromValidHex(initial), reflectInput, reflectOutput, BitVector.fromValidHex(finalXor))

  test("table based implementation should have the same result as bitwise implementation") {
    val poly = hex"04C11DB7".bits
    val table32 = crc(poly, hex"ffffffff".bits, false, false, hex"00000000".bits)
    forAll { (bv: BitVector) => table32(bv) shouldBe crc.bitwise(poly, hex"ffffffff".bits, false, hex"00000000".bits, bv) }
  }

  val checkBytes = BitVector("123456789".getBytes("US-ASCII"))
  def check(crc: BitVector => BitVector, expected: String) =
    crc(checkBytes) shouldBe BitVector.fromValidHex(expected)

  test("CCITT-16") {
    val ccitt16 = crcWith("1021", "ffff", false, false, "0000")
    ccitt16(hex"12345670".bits) shouldBe hex"b1e4".bits
    ccitt16(hex"5a261977".bits) shouldBe hex"1aad".bits
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
}
