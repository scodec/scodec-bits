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
    forAll { (bv: BitVector) => table32(bv) shouldBe crc.bitwise(poly, hex"ffffffff".bits, false, false, hex"00000000".bits, bv) }
  }

  val checkBytes = BitVector("123456789".getBytes("US-ASCII"))
  def check(crc: BitVector => BitVector, expected: String) =
    crc(checkBytes) shouldBe BitVector.fromValidHex(expected)

  test("CRC-3/ROHC") {
    val crc3rohc = crc(bin"011", bin"111", true, true, bin"000")
    crc3rohc(checkBytes) shouldBe bin"110"
  }

  test("CRC-4/ITU") {
    val crc4itu = crc(bin"0011", bin"0000", true, true, bin"0000")
    crc4itu(checkBytes) shouldBe bin"0111"
  }

  test("CRC-5/EPC") {
    val crc5epc = crc(bin"01001", bin"01001", false, false, bin"00000")
    crc5epc(checkBytes) shouldBe bin"00000"
  }

  test("CRC-5/USB") {
    val crc5usb = crc(bin"00101", bin"11111", true, true, bin"11111")
    crc5usb(checkBytes) shouldBe bin"11001"
  }

  test("CRC-5/ITU") {
    val crc5itu = crc(hex"09".bits.drop(1), hex"00".bits.drop(1), false, false, hex"00".bits.drop(1))
    crc5itu(checkBytes) shouldBe hex"75".bits.drop(1)
  }

  test("CRC-7") {
    val crc7 = crc(hex"15".bits.drop(3), hex"00".bits.drop(3), true, true, hex"00".bits.drop(3))
    crc7(checkBytes) shouldBe hex"07".bits.drop(3)
  }

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

  test("CRC-82/DARC") {
    val crc82darc = crc(
      hex"0308c0111011401440411".bits.drop(6),
      hex"000000000000000000000".bits.drop(6),
      true,
      true,
      hex"000000000000000000000".bits.drop(6))
    crc82darc(checkBytes) shouldBe hex"09ea83f625023801fd612".bits.drop(6)
  }
}
