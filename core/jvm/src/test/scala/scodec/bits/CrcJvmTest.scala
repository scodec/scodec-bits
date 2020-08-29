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

import org.scalacheck.Prop.forAll
import Arbitraries._

class CrcJvmTest extends BitsSuite {

  property("crc32 is consistent with java.util.zip.CRC32") {
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
      println(f"java.util.zip.CRC32 is ${1.0 / ratio}%.2f times faster than scodec.bits.crc.crc32")
    else
      println(f"scodec.bits.crc.crc32 is $ratio%.2f times faster than java.util.zip.CRC32")
  }

  private def time[A](f: => A, iterations: Int = 100000): Long = {
    var result: A = f
    for (_ <- 0 until iterations) result = f
    val start = System.nanoTime
    for (_ <- 0 until iterations) result = f
    System.nanoTime - start
  }
}
