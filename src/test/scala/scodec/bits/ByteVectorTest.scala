package scodec.bits

import org.scalacheck.{Arbitrary, Gen, Shrink}
import Arbitrary.arbitrary
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ByteVectorTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {

  val standardByteVectors: Gen[ByteVector] = for {
    size <- Gen.choose(0, 500)
    bytes <- Gen.listOfN(size, Gen.choose(0, 255))
  } yield ByteVector(bytes: _*)

  val sliceByteVectors: Gen[ByteVector] = arbitrary[Array[Byte]] map { bytes => ByteVector.view(bytes) }

  val byteVectors: Gen[ByteVector] = Gen.oneOf(standardByteVectors, sliceByteVectors)

  implicit val arbitraryByteVectors: Arbitrary[ByteVector] = Arbitrary(byteVectors)

  implicit val shrinkByteVector: Shrink[ByteVector] =
    Shrink[ByteVector] { b =>
      if (b.nonEmpty)
        Stream.iterate(b.take(b.size / 2))(b2 => b2.take(b2.size / 2)).takeWhile(_.nonEmpty) ++ Stream(ByteVector.empty)
      else Stream.empty
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

  test("hex string interpolator") {
    hex"deadbeef" shouldBe deadbeef
    val x = ByteVector.fromValidHex("be")
    hex"dead${x}ef" shouldBe deadbeef
    """hex"deadgg"""" shouldNot compile
  }

  test("toIterable roundtrip") {
    forAll { (b: ByteVector) =>
      b shouldBe ByteVector(b.toIterable)
    }
  }
}
