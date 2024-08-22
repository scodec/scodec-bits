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

package scodec.bits.internal

private[bits] object syntax {

  implicit class IntOps(val i: Int) extends AnyVal {
    def leftRotate(bitCount: Int): Int =
      (i << bitCount) | (i >>> (32 - bitCount))
  }

  implicit final class ByteArrayOps(val src: Array[Byte]) extends AnyVal {
    def copyInto(
        dest: Array[Byte],
        destinationOffset: Int = 0,
        startIndex: Int = 0,
        endIndex: Int = src.size
    ): Array[Byte] = {
      System.arraycopy(src, startIndex, dest, destinationOffset, endIndex - startIndex)
      dest
    }
    def copyOf(length: Int): Array[Byte] = java.util.Arrays.copyOf(src, length)
    def fill(value: Byte): Unit =
      java.util.Arrays.fill(src, value)
    def fill(value: Byte, fromIndex: Int, toIndex: Int): Unit =
      java.util.Arrays.fill(src, fromIndex, toIndex, value)
  }
  implicit final class IntArrayOps(val src: Array[Int]) extends AnyVal {
    def fill(value: Int): Unit =
      java.util.Arrays.fill(src, value)
  }
}
