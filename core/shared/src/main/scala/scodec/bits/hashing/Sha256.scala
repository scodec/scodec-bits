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

private[bits] final class Sha256 extends Hasher {
  import Sha256.BLOCK_LENGTH
  private var messageLength = 0L
  private val unprocessed = Array.fill[Byte](BLOCK_LENGTH)(0)
  private var unprocessedLimit = 0
  private val words = Array.fill[Int](BLOCK_LENGTH)(0)
  // SHA256 initial hash value
  private var h0 = 1779033703
  private var h1 = -1150833019
  private var h2 = 1013904242
  private var h3 = -1521486534
  private var h4 = 1359893119
  private var h5 = -1694144372
  private var h6 = 528734635
  private var h7 = 1541459225

  override def update(input: Array[Byte], offset: Int, byteCount: Int): Unit = {
    messageLength += byteCount
    var pos = offset
    val limit = pos + byteCount
    val unprocessed = this.unprocessed
    val unprocessedLimit = this.unprocessedLimit

    if (unprocessedLimit > 0 && unprocessedLimit + byteCount < BLOCK_LENGTH) {
      // Not enough bytes for a chunk.
      input.copyInto(unprocessed, unprocessedLimit, pos, limit)
      this.unprocessedLimit = unprocessedLimit + byteCount
      return
    } else {
      if (unprocessedLimit > 0) {
        // Process a chunk combining leftover bytes and the input.
        val consumeByteCount = BLOCK_LENGTH - unprocessedLimit
        input.copyInto(unprocessed, unprocessedLimit, pos, pos + consumeByteCount)
        processChunk(unprocessed, 0)
        this.unprocessedLimit = 0
        pos += consumeByteCount
      }
      @tailrec
      def go(): Unit = if (pos >= limit) ()
      else {
        val nextPos = pos + BLOCK_LENGTH

        if (nextPos > limit) {
          // Not enough bytes for a chunk.
          input.copyInto(unprocessed, 0, pos, limit)
          this.unprocessedLimit = limit - pos
          return
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
        (({ val i = input(pos_); pos_ += 1; i } & 0xff) << 24) |
        (({ val i = input(pos_); pos_ += 1; i } & 0xff) << 16) |
        (({ val i = input(pos_); pos_ += 1; i } & 0xff) <<  8) |
        ({  val i = input(pos_); pos_ += 1; i } & 0xff)
        // format: on

    for (w <- 16 until BLOCK_LENGTH) {
      val w15 = words(w - 15)
      val s0 = ((w15 >>> 7) | (w15 << 25)) ^ ((w15 >>> 18) | (w15 << 14)) ^ (w15 >>> 3)
      val w2 = words(w - 2)
      val s1 = ((w2 >>> 17) | (w2 << 15)) ^ ((w2 >>> 19) | (w2 << 13)) ^ (w2 >>> 10)
      val w16 = words(w - 16)
      val w7 = words(w - 7)
      words(w) = w16 + s0 + w7 + s1
    }

    hash(words)
  }
  private def hash(words: Array[Int]): Unit = {
    val localK = Sha256.ROUND_CONSTANTS
    var a = h0
    var b = h1
    var c = h2
    var d = h3
    var e = h4
    var f = h5
    var g = h6
    var h = h7

    // Compression
    for (i <- 0 until BLOCK_LENGTH) {
      // format: off
      val s0 =
        ((a >>>  2) | (a << 30)) ^
        ((a >>> 13) | (a << 19)) ^
        ((a >>> 22) | (a << 10))
      val s1 =
        ((e >>>  6) | (e << 26)) ^
        ((e >>> 11) | (e << 21)) ^
        ((e >>> 25) | (e <<  7))


      val ch = (e & f) ^ (~e & g)
      val maj = (a & b) ^ (a & c) ^ (b & c)

      // format: on
      val t1 = h + s1 + ch + localK(i) + words(i)
      val t2 = s0 + maj

      h = g
      g = f
      f = e
      e = d + t1
      d = c
      c = b
      b = a
      a = t1 + t2
    }
    // update current intermediate hash value
    h0 += a
    h1 += b
    h2 += c
    h3 += d
    h4 += e
    h5 += f
    h6 += g
    h7 += h
  }

  override def digest(): Array[Byte] = {
    val unprocessed = this.unprocessed
    var unprocessedLimit = this.unprocessedLimit
    val messageLengthBits = messageLength * 8

    // padding
    unprocessed(unprocessedLimit) = 0x80.toByte
    unprocessedLimit += 1
    if (unprocessedLimit > 56) {
      unprocessed.fill(0, unprocessedLimit, BLOCK_LENGTH)
      processChunk(unprocessed, 0)
      unprocessed.fill(0, 0, unprocessedLimit)
    } else {
      unprocessed.fill(0, unprocessedLimit, 56)
    }
    // format: off
    unprocessed(56) = (messageLengthBits >>> 56).toByte
    unprocessed(57) = (messageLengthBits >>> 48).toByte
    unprocessed(58) = (messageLengthBits >>> 40).toByte
    unprocessed(59) = (messageLengthBits >>> 32).toByte
    unprocessed(60) = (messageLengthBits >>> 24).toByte
    unprocessed(61) = (messageLengthBits >>> 16).toByte
    unprocessed(62) = (messageLengthBits >>>  8).toByte
    unprocessed(63) = (messageLengthBits       ).toByte
    // format: onn
    processChunk(unprocessed, 0)

    val a = h0
    val b = h1
    val c = h2
    val d = h3
    val e = h4
    val f = h5
    val g = h6
    val h = h7

    reset()

    // format: off
    Array(
      (a >> 24).toByte,
      (a >> 16).toByte,
      (a >>  8).toByte,
      (a      ).toByte,
      (b >> 24).toByte,
      (b >> 16).toByte,
      (b >>  8).toByte,
      (b      ).toByte,
      (c >> 24).toByte,
      (c >> 16).toByte,
      (c >>  8).toByte,
      (c      ).toByte,
      (d >> 24).toByte,
      (d >> 16).toByte,
      (d >>  8).toByte,
      (d      ).toByte,
      (e >> 24).toByte,
      (e >> 16).toByte,
      (e >>  8).toByte,
      (e      ).toByte,
      (f >> 24).toByte,
      (f >> 16).toByte,
      (f >>  8).toByte,
      (f      ).toByte,
      (g >> 24).toByte,
      (g >> 16).toByte,
      (g >>  8).toByte,
      (g      ).toByte,
      (h >> 24).toByte,
      (h >> 16).toByte,
      (h >>  8).toByte,
      (h      ).toByte
    )
    // format: on
  }
  private def reset(): Unit = {
    messageLength = 0L
    unprocessed.fill(0)
    unprocessedLimit = 0
    words.fill(0)

    h0 = 1779033703
    h1 = -1150833019
    h2 = 1013904242
    h3 = -1521486534
    h4 = 1359893119
    h5 = -1694144372
    h6 = 528734635
    h7 = 1541459225
  }

}

private[bits] object Sha256 {
  val BLOCK_LENGTH = 64
  val ROUND_CONSTANTS = Array(
    1116352408, 1899447441, -1245643825, -373957723, 961987163, 1508970993, -1841331548,
    -1424204075, -670586216, 310598401, 607225278, 1426881987, 1925078388, -2132889090, -1680079193,
    -1046744716, -459576895, -272742522, 264347078, 604807628, 770255983, 1249150122, 1555081692,
    1996064986, -1740746414, -1473132947, -1341970488, -1084653625, -958395405, -710438585,
    113926993, 338241895, 666307205, 773529912, 1294757372, 1396182291, 1695183700, 1986661051,
    -2117940946, -1838011259, -1564481375, -1474664885, -1035236496, -949202525, -778901479,
    -694614492, -200395387, 275423344, 430227734, 506948616, 659060556, 883997877, 958139571,
    1322822218, 1537002063, 1747873779, 1955562222, 2024104815, -2067236844, -1933114872,
    -1866530822, -1538233109, -1090935817, -965641998
  )
}
