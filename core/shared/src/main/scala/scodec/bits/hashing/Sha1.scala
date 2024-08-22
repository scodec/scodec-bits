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

private[bits] final class Sha1 extends Hasher {
  private var messageLength = 0L
  private val unprocessed = Array.fill[Byte](64)(0)
  private var unprocessedLimit = 0
  private val words = Array.fill[Int](80)(0)

  // SHA1 initial hash value
  private var h0 = 1732584193
  private var h1 = -271733879
  private var h2 = -1732584194
  private var h3 = 271733878
  private var h4 = -1009589776

  override def update(input: Array[Byte], offset: Int, byteCount: Int): Unit = {
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
        (({val i = input(pos_); pos_ +=1; i}.toInt & 0xff) << 24) |
        (({val i = input(pos_); pos_ +=1; i}.toInt & 0xff) << 16) |
        (({val i = input(pos_); pos_ +=1; i}.toInt & 0xff) << 8) |
        (({val i = input(pos_); pos_ +=1; i}.toInt & 0xff))
        // format: on
    for (w <- 16 until 80)
      words(w) = (words(w - 3) ^ words(w - 8) ^ words(w - 14) ^ words(w - 16)).leftRotate(1)

    var a = h0
    var b = h1
    var c = h2
    var d = h3
    var e = h4

    for (i <- 0 until 80) {
      val a2 = if (i < 20) {
        val f = d ^ (b & (c ^ d))
        val k = 1518500249
        (a.leftRotate(5)) + f + e + k + words(i)
      } else if (i < 40) {
        val f = b ^ c ^ d
        val k = 1859775393
        (a.leftRotate(5)) + f + e + k + words(i)
      } else if (i < 60) {
        val f = (b & c) | (b & d) | (c & d)
        val k = -1894007588
        (a.leftRotate(5)) + f + e + k + words(i)
      } else {
        val f = b ^ c ^ d
        val k = -899497514
        (a.leftRotate(5)) + f + e + k + words(i)
      }

      e = d
      d = c
      c = b.leftRotate(30)
      b = a
      a = a2
    }
    h0 += a
    h1 += b
    h2 += c
    h3 += d
    h4 += e
  }
  override def digest(): Array[Byte] = {
    val unprocessed = this.unprocessed
    var unprocessedLimit = this.unprocessedLimit
    val messageLengthBits = messageLength * 8

    unprocessed(unprocessedLimit) = 0x80.toByte
    unprocessedLimit += 1
    if (unprocessedLimit > 56) {
      unprocessed.fill(0, unprocessedLimit, 64)
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
    // format: on
    processChunk(unprocessed, 0)

    val a = h0
    val b = h1
    val c = h2
    val d = h3
    val e = h4

    reset()

    // format: off
    return Array(
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
      (e      ).toByte
    )
    // format: on
  }

  private def reset(): Unit = {
    messageLength = 0L
    unprocessed.fill(0)
    unprocessedLimit = 0
    words.fill(0)

    h0 = 1732584193
    h1 = -271733879
    h2 = -1732584194
    h3 = 271733878
    h4 = -1009589776
  }

}
