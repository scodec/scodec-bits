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

import java.util.zip.{DataFormatException, Deflater, Inflater}

private[bits] trait ByteVectorCrossPlatform { self: ByteVector =>

  /** Compresses this vector using ZLIB.
    *
    * @param level
    *   compression level, 0-9, with 0 disabling compression and 9 being highest level of
    *   compression -- see `java.util.zip.Deflater` for details
    * @param strategy
    *   compression strategy -- see `java.util.zip.Deflater` for details
    * @param nowrap
    *   if true, ZLIB header and checksum will not be used
    * @param chunkSize
    *   buffer size, in bytes, to use when compressing
    * @group conversions
    */
  final def deflate(
      level: Int = Deflater.DEFAULT_COMPRESSION,
      strategy: Int = Deflater.DEFAULT_STRATEGY,
      nowrap: Boolean = false,
      chunkSize: Int = 4096
  ): ByteVector =
    if (isEmpty) this
    else {
      val deflater = new Deflater(level, nowrap)
      try {
        deflater.setStrategy(strategy)

        val buffer = new Array[Byte](chunkSize.toLong.min(size).toInt)
        def loop(acc: ByteVector, fin: Boolean): ByteVector =
          if ((fin && deflater.finished) || (!fin && deflater.needsInput)) acc
          else {
            val count = deflater.deflate(buffer)
            loop(acc ++ ByteVector(buffer, 0, count), fin)
          }

        var result = ByteVector.empty

        foreachV { v =>
          deflater.setInput(v.toArray)
          result = result ++ loop(ByteVector.empty, false)
        }

        deflater.setInput(Array.empty[Byte])
        deflater.finish()
        result ++ loop(ByteVector.empty, true)
      } finally deflater.end()
    }

  /** Decompresses this vector using ZLIB.
    *
    * @param chunkSize
    *   buffer size, in bytes, to use when compressing
    * @param nowrap
    *   if true, will assume no ZLIB header and checksum
    * @group conversions
    */
  final def inflate(
      chunkSize: Int = 4096,
      nowrap: Boolean = false
  ): Either[DataFormatException, ByteVector] =
    if (isEmpty) Right(this)
    else {
      val arr = toArray

      val inflater = new Inflater(nowrap)
      try {
        inflater.setInput(arr)
        try {
          val buffer = new Array[Byte](chunkSize.min(arr.length))
          def loop(acc: ByteVector): ByteVector =
            if (inflater.finished || inflater.needsInput) acc
            else {
              val count = inflater.inflate(buffer)
              loop(acc ++ ByteVector(buffer, 0, count))
            }
          val inflated = loop(ByteVector.empty)
          if (inflater.finished) Right(inflated)
          else
            Left(
              new DataFormatException(
                "Insufficient data -- inflation reached end of input without completing inflation - " + inflated
              )
            )
        } catch {
          case e: DataFormatException => Left(e)
        }
      } finally inflater.end()
    }
}
