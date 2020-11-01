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

/** Bitwise operations on a value of type `Repr`.
  *
  * @tparam Repr type that supports that supports bitwise operations
  * @tparam Idx numeric index type
  *
  * @groupname bitwise Bitwise Operations
  * @groupprio bitwise 2
  *
  * @define bitwiseOperationsReprDescription value
  * @define minsize The resulting $bitwiseOperationsReprDescription's size is the minimum of this $bitwiseOperationsReprDescription's size and the specified $bitwiseOperationsReprDescription's size.
  */
trait BitwiseOperations[Repr <: BitwiseOperations[Repr, Idx], Idx] {

  /** Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the left `n` bits.
    *
    * @group bitwise
    */
  final def <<(n: Idx): Repr = shiftLeft(n)

  /** Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the left `n` bits.
    *
    * @group bitwise
    */
  def shiftLeft(n: Idx): Repr

  /** Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are sign extended.
    *
    * @group bitwise
    */
  final def >>(n: Idx): Repr = shiftRight(n, true)

  /** Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are low.
    *
    * @group bitwise
    */
  final def >>>(n: Idx): Repr = shiftRight(n, false)

  /** Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the right `n` bits.
    *
    * @param signExtension whether the `n` left-msot bits should take on the value of bit 0
    *
    * @group bitwise
    */
  def shiftRight(n: Idx, signExtension: Boolean): Repr

  /** Returns a $bitwiseOperationsReprDescription of the same size with each bit circularly shifted to the left `n` bits.
    *
    * @group bitwise
    */
  def rotateLeft(n: Idx): Repr

  /** Returns a $bitwiseOperationsReprDescription of the same size with each bit circularly shifted to the right `n` bits.
    *
    * @group bitwise
    */
  def rotateRight(n: Idx): Repr

  /** Returns a bitwise complement of this $bitwiseOperationsReprDescription.
    *
    * @group bitwise
    */
  final def unary_~ : Repr = not

  /** Returns a bitwise complement of this $bitwiseOperationsReprDescription.
    *
    * @group bitwise
    */
  def not: Repr

  /** Returns a bitwise AND of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  final def &(other: Repr): Repr = and(other)

  /** Returns a bitwise AND of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  def and(other: Repr): Repr

  /** Returns a bitwise OR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  final def |(other: Repr): Repr = or(other)

  /** Returns a bitwise OR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  def or(other: Repr): Repr

  /** Returns a bitwise XOR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  final def ^(other: Repr): Repr = xor(other)

  /** Returns a bitwise XOR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  def xor(other: Repr): Repr

  /** Returns a bitwise implication of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  def implies(other: Repr): Repr = not.or(other)

  /** Returns a bitwise if-and-only-if of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  def iff(other: Repr): Repr = xor(other).not

  /** Returns a bitwise NAND of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  def nand(other: Repr): Repr = not.and(other)

  /** Returns a bitwise NOR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
    *
    * $minsize
    *
    * @group bitwise
    */
  def nor(other: Repr): Repr = not.or(other)
}
