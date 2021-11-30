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

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, Uint8Array}
import scala.scalajs.js.typedarray.TypedArrayBuffer
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

private[bits] trait ByteVectorCrossPlatform { self: ByteVector =>
  import ByteVector._

  def copyToUint8Array(dest: Uint8Array, start: Int): Unit =
    copyToUint8Array(dest, start, 0, toIntSize(size))

  def copyToUint8Array(dest: Uint8Array, start: Int, offset: Long, size: Int): Unit =
    copyToJSArrayBuffer(dest.buffer, start + dest.byteOffset, offset, dest.byteLength.min(size))

  def toUint8Array: Uint8Array = new Uint8Array(toJSArrayBuffer)

  def copyToJSArrayBuffer(dest: ArrayBuffer, start: Int): Unit =
    copyToJSArrayBuffer(dest, start, 0, toIntSize(size))

  def copyToJSArrayBuffer(dest: ArrayBuffer, start: Int, offset: Long, size: Int): Unit = {
    var i = 0
    val n = dest.byteLength.min(size)
    val out = new Int8Array(dest)
    while (i < n) {
      out(start + i) = self(offset + i)
      i += 1
    }
  }

  def toJSArrayBuffer: ArrayBuffer = this match {
    case Chunk(v) =>
      val bb = v.asByteBuffer
      if (bb.hasArrayBuffer())
        bb.arrayBuffer()
      else {
        val ab = new ArrayBuffer(bb.remaining())
        TypedArrayBuffer.wrap(ab).put(bb)
        ab
      }
    case _ =>
      val dest = new ArrayBuffer(toIntSize(size))
      self.copyToJSArrayBuffer(dest, 0)
      dest
  }
}
