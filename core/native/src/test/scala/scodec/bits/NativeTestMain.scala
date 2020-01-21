package scodec.bits

object NativeTestMain {

  implicit class Ops[A](private val self: A) extends AnyVal {
    def shouldBe(that: A) =
      if (self == that) {
        println(Console.GREEN + s"$self == $that" + Console.RESET)
      } else {
        sys.error(s"$self != $that")
      }
  }

  def main(args: Array[String]): Unit = {

    {
      val b = ByteVector(0x80)
      assert(b.compact eq b.compact)
    }

    {
      val b = ByteVector.empty
      b.insert(0, 1) shouldBe ByteVector(1)
      ByteVector(1, 2, 3, 4).insert(0, 0) shouldBe ByteVector(0, 1, 2, 3, 4)
      ByteVector(1, 2, 3, 4).insert(1, 0) shouldBe ByteVector(1, 0, 2, 3, 4)
    }

    {
      val b1 = ByteVector(0, 1, 2, 3)
      val b2 = ByteVector(1, 2, 3, 4)
      b1.zipWithI(b2)(_ + _) shouldBe ByteVector(1, 3, 5, 7)
    }

    {
      val b1 = ByteVector(0, 1, 2, 3)
      val b2 = ByteVector(1, 2, 3, 4)
      val b3 = ByteVector(2, 3, 4, 5)
      b1.zipWithI2(b2, b3)(_ + _ + _) shouldBe ByteVector(3, 6, 9, 12)
    }

    {
      val b1 = ByteVector(0, 1, 2, 3)
      val b2 = ByteVector(1, 2, 3, 4)
      val b3 = ByteVector(2, 3, 4, 5)
      val b4 = ByteVector(3, 4, 5, 6)
      b1.zipWithI3(b2, b3, b4)(_ + _ + _ + _) shouldBe ByteVector(6, 10, 14, 18)
    }

    val deadbeef = ByteVector(0xde, 0xad, 0xbe, 0xef)
    deadbeef.toHex shouldBe "deadbeef"

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

    ByteVector.fromHexDescriptive("garbage") shouldBe Left(
      "Invalid hexadecimal character 'g' at index 0"
    )
    ByteVector.fromHexDescriptive("deadbefg") shouldBe Left(
      "Invalid hexadecimal character 'g' at index 7"
    )

    deadbeef.toBin shouldBe "11011110101011011011111011101111"

    ByteVector.fromBinDescriptive(deadbeef.toBin) shouldBe Right(deadbeef)
    ByteVector.fromBinDescriptive(deadbeef.toBin.grouped(4).mkString(" ")) shouldBe Right(deadbeef)
    ByteVector.fromBinDescriptive("0001 0011") shouldBe Right(ByteVector(0x13))
    ByteVector.fromBinDescriptive("0b 0001 0011 0111") shouldBe Right(ByteVector(0x01, 0x37))
    ByteVector.fromBinDescriptive("1101a000") shouldBe Left(
      "Invalid binary character 'a' at index 4"
    )
    ByteVector.fromBinDescriptive("0b1101a000") shouldBe Left(
      "Invalid binary character 'a' at index 6"
    )
    ByteVector.fromBinDescriptive("0B1101a000") shouldBe Left(
      "Invalid binary character 'a' at index 6"
    )

    ByteVector.fromValidBin(deadbeef.toBin) shouldBe deadbeef

    val base64 =
      "1MOyoQIABAAAAAAAAAAAAP//AAABAAAAPl6hVQvgDAA8AAAAPAAAAP///////wAhQwjkUwgARQAA\r\nKEPjAABAEd9lqf4Bgan+Af/a/hOIABSGXENNRAAAAAAbqf4B/wAAAAAAAD9eoVX52QYAPAAAADwA\r\nAAABgMIAAAAAH5AHOpIAJkJCAwAAAAAAkAAADlgwS+AAAAA3kAAADlgwS+CAAgIABgABAAQAc2Vy\r\nYwAAAAA="
    BitVector.fromBase64Descriptive(base64).right.map { _.size } shouldBe Right(1408)

    ByteVector(0x55, 0x55, 0x55) << 1 shouldBe ByteVector(0xaa, 0xaa, 0xaa)

    ByteVector(0x55, 0x55, 0x55) >> 1 shouldBe ByteVector(0x2a, 0xaa, 0xaa)
    ByteVector(0xaa, 0xaa, 0xaa) >> 1 shouldBe ByteVector(0xd5, 0x55, 0x55)

    ByteVector(0x55, 0x55, 0x55) >>> 1 shouldBe ByteVector(0x2a, 0xaa, 0xaa)
    ByteVector(0xaa, 0xaa, 0xaa) >>> 1 shouldBe ByteVector(0x55, 0x55, 0x55)

    {
      hex"deadbeef" shouldBe deadbeef
      val x = ByteVector.fromValidHex("be")
      hex"dead${x}ef" shouldBe deadbeef
    }

    {
      val huge = ByteVector.fill(Int.MaxValue * 2L)(0)
      val huge2 = huge ++ huge ++ hex"deadbeef"
      huge2.takeRight(2) shouldBe hex"beef"
    }

    hex"0011223344".take(3) shouldBe hex"001122"
    hex"0011223344".take(1000) shouldBe hex"0011223344"
    hex"0011223344".take(-10) shouldBe hex""

    hex"0011223344".drop(3) shouldBe hex"3344"
    hex"0011223344".drop(-10) shouldBe hex"0011223344"
    hex"0011223344".drop(1000) shouldBe hex""

    hex"001122334455".slice(1, 4) shouldBe hex"112233"
    hex"001122334455".slice(-21, 4) shouldBe hex"00112233"
    hex"001122334455".slice(-21, -4) shouldBe hex""

    {
      val ByteVector(x, y, z) = hex"000102"
      x shouldBe 0.toByte
      y shouldBe 1.toByte
      z shouldBe 2.toByte

      hex"000102" match {
        case ByteVector(0, 1, 2) => // OK
      }
    }
  }
}
