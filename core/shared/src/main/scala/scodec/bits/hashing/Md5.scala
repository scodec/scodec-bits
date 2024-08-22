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
package hashing

import scodec.bits.internal.syntax.*

import scala.annotation.tailrec

private[bits] final class Md5 extends Hasher {
  private var messageLength = 0L
  private val unprocessed = Array.fill[Byte](64)(0)
  private var unprocessedLimit = 0
  private val words = Array.fill[Int](16)(0)
  private var h0: Int = 1732584193
  private var h1: Int = -271733879
  private var h2: Int = -1732584194
  private var h3: Int = 271733878

  override def update(
      input: Array[Byte],
      offset: Int,
      byteCount: Int
  ): Unit = {
    messageLength += byteCount
    var pos = offset
    val limit = pos + byteCount
    val unprocessed = this.unprocessed
    val unprocessedLimit = this.unprocessedLimit

    if (unprocessedLimit > 0 && unprocessedLimit + byteCount < 64) {
      // Not enough bytes for a chunk.
      input.copyInto(unprocessed, unprocessedLimit, pos, limit)
      this.unprocessedLimit = unprocessedLimit + byteCount
      return
    } else {
      if (unprocessedLimit > 0) {
        // Process a chunk combining leftover bytes and the input.
        val consumeByteCount = 64 - unprocessedLimit
        input.copyInto(unprocessed, unprocessedLimit, pos, pos + consumeByteCount)
        processChunk(unprocessed, 0)
        this.unprocessedLimit = 0
        pos += consumeByteCount
      }

      @tailrec
      def go(): Unit = if (pos >= limit) ()
      else {
        val nextPos = pos + 64
        if (nextPos > limit) {

          // Not enough bytes for a chunk.
          input.copyInto(unprocessed, 0, pos, limit)
          this.unprocessedLimit = limit - pos
        } else {
          // Process a chunk.
          processChunk(input, pos)
          pos = nextPos
          go()
        }
      }
      go()
    }
  }

  private def processChunk(input: Array[Byte], pos: Int): Unit = {
    val words = this.words

    var pos_ = pos
    for (w <- 0 until 16)
      words(w) =
        // format: off
        ({ val i = input(pos_); pos_ += 1; i }.toInt & 0xff) |
        (({ val i = input(pos_); pos_ += 1; i }.toInt & 0xff) << 8) |
        (({ val i = input(pos_); pos_ += 1; i }.toInt & 0xff) << 16) |
        (({ val i = input(pos_); pos_ += 1; i }.toInt & 0xff) << 24)
        // format: on
    hash(words)
  }

  private def hash(words: Array[Int]): Unit = {
    val localK = Md5.k
    val localS = Md5.s

    var a = h0
    var b = h1
    var c = h2
    var d = h3

    for (i <- 0 until 16) {
      val g = i
      val f = ((b & c) | (~b & d)) + a + localK(i) + words(g)
      a = d
      d = c
      c = b
      b += f.leftRotate(localS(i))
    }

    for (i <- 16 until 32) {
      val g = ((5 * i) + 1) % 16
      val f = ((d & b) | (~d & c)) + a + localK(i) + words(g)
      a = d
      d = c
      c = b
      b += f.leftRotate(localS(i))
    }

    for (i <- 32 until 48) {
      val g = ((3 * i) + 5) % 16
      val f = (b ^ c ^ d) + a + localK(i) + words(g)
      a = d
      d = c
      c = b
      b += f.leftRotate(localS(i))
    }

    for (i <- 48 until 64) {
      val g = (7 * i) % 16
      val f = (c ^ (b | ~d)) + a + localK(i) + words(g)
      a = d
      d = c
      c = b
      b += f.leftRotate(localS(i))
    }

    h0 += a
    h1 += b
    h2 += c
    h3 += d
  }

  // format: off
  override def digest(): Array[Byte] = {
    val messageLengthBits = messageLength * 8

    unprocessed(unprocessedLimit) = 0x80.toByte
    unprocessedLimit += 1

    if (unprocessedLimit > 56) {
      java.util.Arrays.fill(unprocessed, unprocessedLimit, 64,0.toByte)
      processChunk(unprocessed, 0)
      java.util.Arrays.fill(unprocessed, 0, unprocessedLimit, 0.toByte)
    } else {
      java.util.Arrays.fill(unprocessed, unprocessedLimit, 56,0.toByte)
    }
    unprocessed(56) = (messageLengthBits       ).toByte
    unprocessed(57) = (messageLengthBits >>>  8).toByte
    unprocessed(58) = (messageLengthBits >>> 16).toByte
    unprocessed(59) = (messageLengthBits >>> 24).toByte
    unprocessed(60) = (messageLengthBits >>> 32).toByte
    unprocessed(61) = (messageLengthBits >>> 40).toByte
    unprocessed(62) = (messageLengthBits >>> 48).toByte
    unprocessed(63) = (messageLengthBits >>> 56).toByte
    processChunk(unprocessed, 0)

    val a = h0
    val b = h1
    val c = h2
    val d = h3

    return Array[Byte](
      (a      ).toByte,
      (a >>  8).toByte,
      (a >> 16).toByte,
      (a >> 24).toByte,
      (b      ).toByte,
      (b >>  8).toByte,
      (b >> 16).toByte,
      (b >> 24).toByte,
      (c      ).toByte,
      (c >>  8).toByte,
      (c >> 16).toByte,
      (c >> 24).toByte,
      (d      ).toByte,
      (d >>  8).toByte,
      (d >> 16).toByte,
      (d >> 24).toByte
    )
  }
  // format: on

}

object Md5 {
    // format: off
    // per-round shift amounts
    private val s = Array[Int](
      7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9,
      14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15,
      21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
    )

    private val k = Array[Int](
      -680876936, -389564586, 606105819, -1044525330, -176418897, 1200080426, -1473231341,
      -45705983, 1770035416, -1958414417, -42063, -1990404162, 1804603682, -40341101, -1502002290,
      1236535329, -165796510, -1069501632, 643717713, -373897302, -701558691, 38016083, -660478335,
      -405537848, 568446438, -1019803690, -187363961, 1163531501, -1444681467, -51403784,
      1735328473, -1926607734, -378558, -2022574463, 1839030562, -35309556, -1530992060, 1272893353,
      -155497632, -1094730640, 681279174, -358537222, -722521979, 76029189, -640364487, -421815835,
      530742520, -995338651, -198630844, 1126891415, -1416354905, -57434055, 1700485571,
      -1894986606, -1051523, -2054922799, 1873313359, -30611744, -1560198380, 1309151649,
      -145523070, -1120210379, 718787259, -343485551,
    )
    // format: on
}
