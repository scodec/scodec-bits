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

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ScodecBitsBenchmark {

  val N = 100000L
  val M = 1024L

  val bitChunks_N = (0L until N).map(b => BitVector(b.toByte)).toList
  val byteChunks_N = (0L until N).map(b => ByteVector(b.toByte)).toList
  val bytes_N = Array.tabulate(N.toInt)(i => i.toByte)
  val bitVector_N = bitChunks_N.foldLeft(BitVector.empty)(_ ++ _)
  val byteVector_N = byteChunks_N.foldLeft(ByteVector.empty)(_ ++ _)

  val bitChunks_M = (0L until M).map(b => BitVector(b.toByte)).toList
  val byteChunks_M = (0L until M).map(b => ByteVector(b.toByte)).toList
  val bytes_M = Array.tabulate(M.toInt)(i => i.toByte)
  val bitVector_M = bitChunks_M.foldLeft(BitVector.empty)(_ ++ _)
  val bitVector_M_compact = bitVector_M.copy
  val byteVector_M = byteChunks_M.foldLeft(ByteVector.empty)(_ ++ _)

  @Benchmark def listCons_N(): Int =
    bitChunks_N.foldLeft(List[BitVector]())((t, h) => h :: t).size
  @Benchmark def vectorSnoc_N(): Int =
    bitChunks_N.foldLeft(Vector[BitVector]())((a, b) => a :+ b).size
  @Benchmark def listCons_M(): Int =
    bitChunks_M.foldLeft(List[BitVector]())((t, h) => h :: t).size
  @Benchmark def vectorSnoc_M(): Int =
    bitChunks_M.foldLeft(Vector[BitVector]())((a, b) => a :+ b).size

  // N
  @Benchmark def bitVectorAppendSnoc_N(): Long =
    bitChunks_N.foldLeft(BitVector.empty)(_ ++ _).size
  @Benchmark def byteVectorAppendSnoc_N(): Long =
    byteChunks_N.foldLeft(ByteVector.empty)(_ ++ _).size
  @Benchmark def byteVectorSnoc_N(): Long =
    bytes_N.foldLeft(ByteVector.empty)(_ :+ _).size
  @Benchmark def byteVectorSnocUnboxed_N(): Long = {
    var b = ByteVector.empty
    var i = 0
    while (i < bytes_N.length) {
      b = b :+ bytes_N(i)
      i += 1
    }
    b.size
  }
  @Benchmark def bitVectorStride_N(): Long =
    (0L until (N / 512)).foldLeft(bitVector_N)((b, _) => b.drop(512 * 8)).size
  @Benchmark def byteVectorStride_N(): Long =
    (0L until (N / 512)).foldLeft(byteVector_N)((b, _) => b.drop(512)).size
  @Benchmark def bitVectorTake_N(): Long =
    (N until 0L by -512L).foldLeft(bitVector_N)((b, n) => b.take(n)).size
  @Benchmark def byteVectorTake_N(): Long =
    (N until 0 by -512).foldLeft(byteVector_N)((b, n) => b.take(n)).size

  // M
  @Benchmark def bitVectorAppendSnoc_M(): Long =
    bitChunks_M.foldLeft(BitVector.empty)(_ ++ _).size
  @Benchmark def byteVectorAppendSnoc_M(): Long =
    byteChunks_M.foldLeft(ByteVector.empty)(_ ++ _).size
  @Benchmark def byteVectorSnoc_M(): Long =
    bytes_M.foldLeft(ByteVector.empty)(_ :+ _).size
  @Benchmark def byteVectorSnocUnboxed_M(): Long = {
    var b = ByteVector.empty
    var i = 0
    while (i < bytes_M.length) {
      b = b :+ bytes_M(i)
      i += 1
    }
    b.size
  }
  @Benchmark def bitVectorStride_M(): Long =
    (0L until (M / 512)).foldLeft(bitVector_M)((b, _) => b.drop(512L * 8)).size
  @Benchmark def byteVectorStride_M(): Long =
    (0L until (M / 512)).foldLeft(byteVector_M)((b, _) => b.drop(512L)).size
  @Benchmark def bitVectorTake_M(): Long =
    (M until 0L by -512L).foldLeft(bitVector_M)((b, n) => b.take(n)).size
  @Benchmark def byteVectorTake_M(): Long =
    (M until 0 by -512).foldLeft(byteVector_M)((b, n) => b.take(n)).size

  @Benchmark def toBase64(): String =
    bitVector_M.toBase64
  @Benchmark def toBase64_compact(): String =
    bitVector_M_compact.toBase64
  @Benchmark def toBase64_JRE(): String =
    java.util.Base64.getEncoder.encodeToString(bitVector_M.toByteArray)
  @Benchmark def toBase64_JRE_compact(): String =
    java.util.Base64.getEncoder.encodeToString(bitVector_M_compact.toByteArray)

  private val bitVector_M_b64 = bitVector_M.toBase64
  @Benchmark def fromBase64(): Option[ByteVector] =
    ByteVector.fromBase64(bitVector_M_b64)
  @Benchmark def fromBase64_JRE(): Array[Byte] =
    java.util.Base64.getDecoder.decode(bitVector_M_b64)

  private val crc32 = crc(hex"04c11db7".bits, hex"ffffffff".bits, true, true, hex"ffffffff".bits)
  private val crc32v =
    crc.vectorTable(hex"04c11db7".bits, hex"ffffffff".bits, true, true, hex"ffffffff".bits)
  private val crc32i =
    crc.int32(0x04c11db7, 0xffffffff, true, true, 0xffffffff).andThen(i => BitVector.fromInt(i))
  @Benchmark def crc32_M(): BitVector = crc32(bitVector_M)
  @Benchmark def crc32v_M(): BitVector = crc32v(bitVector_M)
  @Benchmark def crc32i_M(): BitVector = crc32i(bitVector_M)
}
