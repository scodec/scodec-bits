package scodec.bits

import Arbitraries._

class CrcJvmTest extends BitsSuite {

  test("crc32 is consistent with java.util.zip.CRC32") {
    forAll { (b: ByteVector) =>
      assert(crc.crc32(b.bits).bytes == {
        val c = new java.util.zip.CRC32
        c.update(b.toArray)
        ByteVector.fromLong(c.getValue, size = 4)
      })
    }
  }

  test("performance of crc32 should be comparable with java.util.zip.CRC32") {
    val data = Array.fill[Byte](256)(42)
    val timeTableBased = time(crc.crc32(BitVector.view(data)))
    val timeJava = time {
      val c = new java.util.zip.CRC32
      c.update(data)
      c.getValue
    }
    val ratio = timeJava.toDouble / timeTableBased.toDouble
    if (ratio < 1.0)
      info(f"java.util.zip.CRC32 is ${1.0 / ratio}%.2f times faster than scodec.bits.crc.crc32")
    else
      info(f"scodec.bits.crc.crc32 is $ratio%.2f times faster than java.util.zip.CRC32")
  }

  private def time[A](f: => A, iterations: Int = 100000): Long = {
    var result: A = f
    for (_ <- 0 until iterations) result = f
    val start = System.nanoTime
    for (_ <- 0 until iterations) result = f
    System.nanoTime - start
  }
}
