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

/** Provides support for calculating cyclic redundancy checks.
  *
  * @see
  *   http://www.repairfaq.org/filipg/LINK/F_crc_v3.html
  */
object crc {

  /** 32-bit CRC using poly 0x04c11db7, initial 0xffffffff, reflected input/output, and final xor
    * 0xffffffff.
    */
  lazy val crc32: BitVector => BitVector =
    crc32Builder.updated(_).result

  /** 32-bit CRC using poly 0x1edc6f41, initial 0xffffffff, reflected input/output, and final xor
    * 0xffffffff.
    */
  lazy val crc32c: BitVector => BitVector =
    crc32cBuilder.updated(_).result

  /** Builder for 32-bit CRC using poly 0x04c11db7, initial 0xffffffff, reflected input/output, and
    * final xor 0xffffffff.
    */
  lazy val crc32Builder: CrcBuilder[BitVector] =
    builder32(0x04c11db7, 0xffffffff, true, true, 0xffffffff).mapResult(BitVector.fromInt(_))

  /** Builder for 32-bit CRC using poly 0x1edc6f41, initial 0xffffffff, reflected input/output, and
    * final xor 0xffffffff.
    */
  lazy val crc32cBuilder: CrcBuilder[BitVector] =
    builder32(0x1edc6f41, 0xffffffff, true, true, 0xffffffff).mapResult(BitVector.fromInt(_))

  /** An immutable "builder" to incrementally compute a CRC.
    */
  sealed trait CrcBuilder[R] {
    def updated(data: BitVector): CrcBuilder[R]
    def result: R

    private[crc] def mapResult[S](f: R => S): CrcBuilder[S] = {
      class Builder(inner: CrcBuilder[R]) extends CrcBuilder[S] {
        def updated(data: BitVector): CrcBuilder[S] = new Builder(inner.updated(data))
        def result: S = f(inner.result)
      }
      new Builder(this)
    }
  }

  /** Constructs a table-based CRC function using the specified polynomial.
    *
    * Each of the input vectors must be the same size.
    *
    * @return
    *   function that calculates a `n`-bit CRC where `n = poly.size`
    */
  def apply(
      poly: BitVector,
      initial: BitVector,
      reflectInput: Boolean,
      reflectOutput: Boolean,
      finalXor: BitVector
  ): BitVector => BitVector = {
    require(poly.nonEmpty, "empty polynomial")
    require(
      initial.size == poly.size && poly.size == finalXor.size,
      "poly, initial, and finalXor must be same length"
    )

    if (poly.size == 32L)
      int32(poly.toInt(), initial.toInt(), reflectInput, reflectOutput, finalXor.toInt()).andThen {
        i => BitVector.fromInt(i)
      }
    else {
      val b = builder(poly, initial, reflectInput, reflectOutput, finalXor)
      b.updated(_).result
    }
  }

  /** Constructs a table-based CRC builder using the specified polynomial. */
  def builder(
      poly: BitVector,
      initial: BitVector,
      reflectInput: Boolean,
      reflectOutput: Boolean,
      finalXor: BitVector
  ): CrcBuilder[BitVector] =
    if (poly.size == 32L)
      builder32(poly.toInt(), initial.toInt(), reflectInput, reflectOutput, finalXor.toInt())
        .mapResult(BitVector.fromInt(_))
    else builderGeneric(poly, initial, reflectInput, reflectOutput, finalXor)

  private[bits] def builderGeneric(
      poly: BitVector,
      initial: BitVector,
      reflectInput: Boolean,
      reflectOutput: Boolean,
      finalXor: BitVector
  ): CrcBuilder[BitVector] = {
    val table = new Array[BitVector](256)
    val zeroed = BitVector.fill(poly.size - 8)(false)
    val m = 8L
    @annotation.tailrec
    def calculateTableIndex(idx: Int): Unit =
      if (idx < table.size) {
        @annotation.tailrec
        def shift(k: Int, crcreg: BitVector): BitVector =
          if (k < m)
            shift(
              k + 1, {
                val shifted = crcreg << 1
                if (crcreg.head) shifted.xor(poly) else shifted
              }
            )
          else crcreg
        table(idx) = shift(0, ByteVector(idx).bits ++ zeroed).compact
        calculateTableIndex(idx + 1)
      }
    calculateTableIndex(0)

    final class Builder(initial: BitVector) extends CrcBuilder[BitVector] {

      def updated(input: BitVector): Builder =
        if (poly.size < 8)
          new Builder(
            goBitwise(poly, if (reflectInput) input.reverseBitOrder else input, initial)
          )
        else {
          var crcreg = initial
          val size = input.size
          val byteAligned = size % 8 == 0
          val data = if (byteAligned) input.bytes else input.bytes.init
          if (reflectInput)
            data.foreach { inputByte =>
              val index = crcreg.take(8) ^ BitVector(inputByte).reverse
              val indexAsInt = index.bytes.head.toInt & 0x0ff
              crcreg = (crcreg << 8) ^ table(indexAsInt)
            }
          else
            data.foreach { inputByte =>
              val index = crcreg.take(8) ^ BitVector(inputByte)
              val indexAsInt = index.bytes.head.toInt & 0x0ff
              crcreg = (crcreg << 8) ^ table(indexAsInt)
            }
          if (byteAligned)
            new Builder(crcreg)
          else {
            val trailer = input.takeRight(size % 8)
            new Builder(
              goBitwise(poly, if (reflectInput) trailer.reverseBitOrder else trailer, crcreg)
            )
          }
        }

      def result: BitVector = (if (reflectOutput) initial.reverse else initial).xor(finalXor)
    }

    new Builder(initial)
  }

  private def goBitwise(poly: BitVector, remaining: BitVector, crcreg: BitVector): BitVector =
    if (remaining.isEmpty) crcreg
    else
      goBitwise(
        poly,
        remaining.tail, {
          val shifted = crcreg << 1
          if (crcreg.head == remaining.head) shifted else shifted.xor(poly)
        }
      )

  /** Constructs a 32-bit, table-based CRC function using the specified polynomial.
    *
    * @return
    *   function that calculates a 32-bit CRC
    */
  def int32(
      poly: Int,
      initial: Int,
      reflectInput: Boolean,
      reflectOutput: Boolean,
      finalXor: Int
  ): BitVector => Int = {
    val b = builder32(poly, initial, reflectInput, reflectOutput, finalXor)
    b.updated(_).result
  }

  /** Constructs a 32-bit, table-based CRC builder using the specified polynomial.
    */
  def builder32(
      poly: Int,
      initial: Int,
      reflectInput: Boolean,
      reflectOutput: Boolean,
      finalXor: Int
  ): CrcBuilder[Int] = {
    val table = new Array[Int](256)
    @annotation.tailrec
    def calculateTableIndex(idx: Int): Unit =
      if (idx < table.size) {
        @annotation.tailrec
        def shift(k: Int, crcreg: Int): Int =
          if (k < 8)
            shift(
              k + 1, {
                val shifted = crcreg << 1
                if ((crcreg & 0x80000000) != 0) shifted ^ poly else shifted
              }
            )
          else crcreg
        table(idx) = shift(0, idx << 24)
        calculateTableIndex(idx + 1)
      }
    calculateTableIndex(0)

    final class Builder(initial: Int) extends CrcBuilder[Int] {
      def updated(input: BitVector): Builder = {
        var crcreg = initial
        val size = input.size
        val byteAligned = size % 8 == 0
        val data = if (byteAligned) input.bytes else input.bytes.init
        if (reflectInput)
          data.foreach { inputByte =>
            val index = (crcreg >>> 24) ^ (BitVector.reverseBitsInByte(inputByte) & 0xff)
            crcreg = (crcreg << 8) ^ table(index)
          }
        else
          data.foreach { inputByte =>
            val index = (crcreg >>> 24) ^ (inputByte & 0xff)
            crcreg = (crcreg << 8) ^ table(index)
          }
        if (byteAligned)
          new Builder(crcreg)
        else {
          val trailer = input.takeRight(size % 8)
          new Builder(
            goBitwise(poly, if (reflectInput) trailer.reverseBitOrder else trailer, crcreg)
          )
        }
      }

      def result: Int =
        (if (reflectOutput) BitVector.fromInt(initial).reverse.toInt() else initial) ^ finalXor
    }

    new Builder(initial)
  }

  private def goBitwise(poly: Int, remaining: BitVector, crcreg: Int): Int =
    if (remaining.isEmpty) crcreg
    else
      goBitwise(
        poly,
        remaining.tail, {
          val shifted = crcreg << 1
          if (((crcreg & 0x80000000) != 0) == remaining.head) shifted else shifted ^ poly
        }
      )

  /** Calculates a bitwise CRC of the specified value.
    *
    * If calculating a lot of CRCs, prefer the `apply` method, which precomputes a lookup table and
    * uses it in each CRC calculation.
    *
    * @return
    *   function that calculates a `n`-bit CRC where `n = poly.size`
    */
  def bitwise(
      poly: BitVector,
      initial: BitVector,
      reflectInput: Boolean,
      reflectOutput: Boolean,
      finalXor: BitVector,
      value: BitVector
  ): BitVector = {
    val reg = goBitwise(poly, if (reflectInput) value.reverseBitOrder else value, initial)
    (if (reflectOutput) reg.reverse else reg).xor(finalXor)
  }
}
