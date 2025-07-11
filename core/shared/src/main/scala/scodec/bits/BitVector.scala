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

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.{CharacterCodingException, Charset}
import java.util.UUID

/** Persistent vector of bits, stored as bytes.
  *
  * Bits are numbered left to right, starting at 0.
  *
  * @groupname collection
  *   Collection Like Methods
  * @groupprio collection
  *   0
  *
  * @groupname bitwise
  *   Bitwise Operations
  * @groupprio bitwise
  *   1
  *
  * @groupname conversions
  *   Conversions
  * @groupprio conversions
  *   2
  *
  * @groupname crypto
  *   Cryptography
  * @groupprio crypto
  *   3
  *
  * @define bitwiseOperationsReprDescription
  *   bit vector
  */
sealed abstract class BitVector
    extends BitwiseOperations[BitVector, Long]
    with BitVectorCrossPlatform
    with Ordered[BitVector]
    with Serializable {
  import BitVector._

  /** Returns number of bits in this vector.
    *
    * @group collection
    */
  def size: Long

  /** Alias for [[size]].
    * @group collection
    */
  final def length: Long = size

  /** Returns true if this vector has no bits.
    *
    * @group collection
    */
  final def isEmpty: Boolean = sizeLessThan(1)

  /** Returns true if this vector has a non-zero number of bits.
    *
    * @group collection
    */
  final def nonEmpty: Boolean = !isEmpty

  /** Returns `true` if the size of this `BitVector` is greater than `n`. Unlike `size`, this forces
    * this `BitVector` from left to right, halting as soon as it has a definite answer.
    *
    * @group collection
    */
  final def sizeGreaterThan(n: Long): Boolean = n < 0 || !sizeLessThanOrEqual(n)

  /** Returns `true` if the size of this `BitVector` is greater than or equal to `n`. Unlike `size`,
    * this forces this `BitVector` from left to right, halting as soon as it has a definite answer.
    *
    * @group collection
    */
  final def sizeGreaterThanOrEqual(n: Long): Boolean = n < 0 || !sizeLessThanOrEqual(n - 1)

  /** Returns `true` if the size of this `BitVector` is less than `n`. Unlike `size`, this forces
    * this `BitVector` from left to right, halting as soon as it has a definite answer.
    *
    * @group collection
    */
  def sizeLessThan(n: Long): Boolean

  /** Returns `true` if the size of this `BitVector` is less than or equal to `n`. Unlike `size`,
    * this forces this `BitVector` from left to right, halting as soon as it has a definite answer.
    *
    * @group collection
    */
  final def sizeLessThanOrEqual(n: Long): Boolean =
    n == Long.MaxValue || sizeLessThan(n + 1)

  /** Returns the number of bits in this vector, or `None` if the size does not fit into an `Int`.
    *
    * @group collection
    */
  final def intSize: Option[Int] = if (size <= Int.MaxValue) Some(size.toInt) else None

  /** Returns true if the `n`th bit is high, false otherwise.
    *
    * @throws NoSuchElementException
    *   if `n >= size`
    *
    * @group collection
    */
  def get(n: Long): Boolean

  /** Returns the `n`th byte, 0-indexed.
    *
    * @throws NoSuchElementException
    *   if `n >= bytes.size`
    *
    * @group collection
    */
  def getByte(n: Long): Byte

  /** Alias for `get`.
    *
    * @group collection
    * @see
    *   get(Long)
    */
  final def apply(n: Long): Boolean = get(n)

  /** Returns `Some(true)` if the `n`th bit is high, `Some(false)` if low, and `None` if `n >=
    * size`.
    *
    * @group collection
    */
  final def lift(n: Long): Option[Boolean] =
    if (sizeGreaterThan(n)) Some(get(n))
    else None

  private[bits] def unchunk = this

  /** Returns a new bit vector with the `n`th bit high if `high` is true or low if `high` is false.
    *
    * @group collection
    */
  def update(n: Long, high: Boolean): BitVector

  /** Returns a vector with the specified bit inserted at the specified index.
    * @group collection
    */
  final def insert(idx: Long, b: Boolean): BitVector =
    (take(idx) :+ b) ++ drop(idx)

  /** Returns a vector with the specified bit vector inserted at the specified index.
    * @group collection
    */
  final def splice(idx: Long, b: BitVector): BitVector =
    take(idx) ++ b ++ drop(idx)

  /** Returns a vector with the specified bit vector replacing bits `[idx, idx + b.size]`.
    * @group collection
    */
  final def patch(idx: Long, b: BitVector): BitVector =
    take(idx) ++ b ++ drop(idx + b.size)

  /** Returns a new bit vector with the `n`th bit high (and all other bits unmodified).
    *
    * @group collection
    */
  final def set(n: Long): BitVector = update(n, true)

  /** Returns a new bit vector with the `n`th bit low (and all other bits unmodified).
    *
    * @group collection
    */
  final def clear(n: Long): BitVector = update(n, false)

  /** Returns a new bit vector representing this vector's contents followed by the specified
    * vector's contents.
    *
    * @group collection
    */
  def ++(b2: BitVector): BitVector =
    if (this.isEmpty) b2
    else Chunks(Append(this, b2))

  /** Returns a new vector with the specified bit prepended.
    * @group collection
    */
  final def +:(b: Boolean): BitVector = BitVector.bit(b) ++ this

  /** Returns a new vector with the specified bit appended.
    * @group collection
    */
  final def :+(b: Boolean): BitVector = this ++ BitVector.bit(b)

  /** Returns the depth of this tree. The result of `compact` has depth 0.
    */
  private[bits] def depth: Int =
    this match {
      case Append(l, r) => 1 + (l.depth.max(r.depth))
      case c: Chunks    => 1 + c.chunks.depth
      case _            => 0
    }

  /** Returns a vector of all bits in this vector except the first `n` bits.
    *
    * The resulting vector's size is `0 max (size - n)`.
    *
    * @group collection
    */
  def drop(n: Long): BitVector

  /** Returns a vector of all bits in this vector except the last `n` bits.
    *
    * The resulting vector's size is `0 max (size - n)`.
    *
    * @group collection
    */
  final def dropRight(n: Long): BitVector =
    if (n <= 0) this
    else if (n >= size) BitVector.empty
    else take(size - n)

  /** Returns a vector of the first `n` bits of this vector.
    *
    * The resulting vector's size is `n min size`.
    *
    * Note: if an `n`-bit vector is required, use the `acquire` method instead.
    *
    * @see
    *   acquire
    * @group collection
    */
  def take(n: Long): BitVector

  /** Returns a vector of the last `n` bits of this vector.
    *
    * The resulting vector's size is `n min size`.
    *
    * @group collection
    */
  final def takeRight(n: Long): BitVector =
    if (n < 0) throw new IllegalArgumentException(s"takeRight($n)")
    else if (n >= size) this
    else this.drop(size - n)

  /** Returns a pair of vectors that is equal to `(take(n), drop(n))`.
    * @group collection
    */
  final def splitAt(n: Long): (BitVector, BitVector) = (take(n), drop(n))

  /** Returns a vector made up of the bits starting at index `from` up to index `until`, not
    * including the index `until`.
    *
    * @group collection
    */
  final def slice(from: Long, until: Long): BitVector =
    drop(from).take(until - (from.max(0)))

  /** Returns an iterator of fixed size slices of this vector, stepping a single bit between consecutive elements.
    *
    * @group collection
    */
  final def sliding(n: Long): Iterator[BitVector] =
    sliding(n, 1L)

  /** Returns an iterator of fixed size slices of this vector, stepping the specified number of bits between consecutive elements.
    *
    * @group collection
    */
  final def sliding(n: Long, step: Long): Iterator[BitVector] = {
    assert(n > 0 && step > 0, "both n and step must be positive")
    def limit(itr: Iterator[Long]): Iterator[Long] =
      if (step < n) itr.take((size - n).toInt + 1) else itr.takeWhile(_ < size)
    limit(Iterator.iterate(0L)(_ + step)).map(idx => slice(idx, idx + n))
  }

  /** Returns a vector whose contents are the results of taking the first `n` bits of this vector.
    *
    * If this vector does not contain at least `n` bits, an error message is returned.
    *
    * @see
    *   take
    * @group collection
    */
  def acquire(n: Long): Either[String, BitVector] =
    if (sizeGreaterThanOrEqual(n)) Right(take(n))
    else Left(s"cannot acquire $n bits from a vector that contains $size bits")

  /** Like `aquire`, but immediately consumes the `Either` via the pair of functions `err` and `f`.
    *
    * @see
    *   acquire
    * @group collection
    */
  final def acquireThen[R](n: Long)(err: String => R, f: BitVector => R): R =
    if (sizeGreaterThanOrEqual(n)) f(take(n))
    else err(s"cannot acquire $n bits from a vector that contains $size bits")

  /** Consumes the first `n` bits of this vector and decodes them with the specified function,
    * resulting in a vector of the remaining bits and the decoded value. If this vector does not
    * have `n` bits or an error occurs while decoding, an error is returned instead.
    *
    * @group collection
    */
  final def consume[A](
      n: Long
  )(decode: BitVector => Either[String, A]): Either[String, (BitVector, A)] =
    for {
      toDecode <- acquire(n)
      decoded <- decode(toDecode)
    } yield (drop(n), decoded)

  /** If this vector has at least `n` bits, returns `f(take(n),drop(n))`, otherwise calls `err` with
    * a meaningful error message. This function can be used to avoid intermediate allocations of
    * `Either` objects when using `acquire` or `consume` directly.
    *
    * @see
    *   acquireThen
    * @group collection
    */
  final def consumeThen[R](n: Long)(err: String => R, f: (BitVector, BitVector) => R): R =
    if (sizeGreaterThanOrEqual(n)) f(take(n), drop(n)) // todo unsafeTake, unsafeDrop
    else err(s"cannot acquire $n bits from a vector that contains $size bits")

  /** Returns true if this bit vector starts with the specified vector.
    * @group collection
    */
  final def startsWith(b: BitVector): Boolean =
    take(b.size) === b

  /** Returns true if this bit vector ends with the specified vector.
    * @group collection
    */
  final def endsWith(b: BitVector): Boolean =
    takeRight(b.size) === b

  /** Finds the first index of the specified bit pattern in this vector.
    * @return
    *   index of slice or -1 if not found
    * @group collection
    */
  final def indexOfSlice(slice: BitVector): Long = indexOfSlice(slice, 0)

  /** Finds the first index after `from` of the specified bit pattern in this vector.
    * @return
    *   index of slice or -1 if not found
    * @group collection
    */
  final def indexOfSlice(slice: BitVector, from: Long): Long = {
    @annotation.tailrec
    def go(b: BitVector, idx: Long): Long =
      if (b.startsWith(slice)) idx
      else if (b.isEmpty) -1
      else go(b.tail, idx + 1)
    go(drop(from), from)
  }

  /** Determines if the specified slice is in this vector.
    * @group collection
    */
  final def containsSlice(slice: BitVector): Boolean = indexOfSlice(slice) >= 0

  // This was public before version 1.1.8 so it must stay here for bincompat
  // The public grouped method is adding via an extension method defined in the companion
  private[bits] final def grouped(n: Long): Stream[BitVector] =
    groupedIterator(n).toStream

  private final def groupedIterator(n: Long): Iterator[BitVector] =
    if (isEmpty) Iterator.empty
    else Iterator(take(n)) ++ drop(n).groupedIterator(n)

  /** Returns the first bit of this vector or throws if vector is emtpy.
    * @group collection
    */
  final def head: Boolean = get(0)

  /** Returns the first bit of this vector or `None` if vector is emtpy.
    * @group collection
    */
  final def headOption: Option[Boolean] = lift(0)

  /** Returns a vector of all bits in this vector except the first bit.
    * @group collection
    */
  final def tail: BitVector = drop(1)

  /** Returns a vector of all bits in this vector except the last bit.
    * @group collection
    */
  final def init: BitVector = dropRight(1)

  /** Returns the last bit in this vector or throws if vector is empty.
    * @group collection
    */
  final def last: Boolean = apply(size - 1)

  /** Returns the last bit in this vector or returns `None` if vector is empty.
    * @group collection
    */
  final def lastOption: Option[Boolean] = lift(size - 1)

  /** Alias for `padRight`.
    *
    * @throws IllegalArgumentException
    *   if `n < size`
    * @group collection
    */
  final def padTo(n: Long): BitVector = padRight(n)

  /** Returns an `n`-bit vector whose contents are 0 or more low bits followed by this vector's
    * contents.
    *
    * @throws IllegalArgumentException
    *   if `n < size`
    * @group collection
    */
  final def padRight(n: Long): BitVector =
    if (n < size)
      throw new IllegalArgumentException(
        s"Trying to right-pad a vector of $size bits to only $n bits."
      )
    else this ++ BitVector.fill(n - size)(false)

  /** Returns an `n`-bit vector whose contents are 0 or more low bits followed by this vector's
    * contents.
    *
    * @throws IllegalArgumentException
    *   if `n < size`
    * @group collection
    */
  final def padLeft(n: Long): BitVector =
    if (n < size)
      throw new IllegalArgumentException(
        s"Trying to left-pad a vector of $size bits to only $n bits."
      )
    else BitVector.fill(n - size)(false) ++ this

  /** Reverse the bits of this vector.
    *
    * @group collection
    */
  final def reverse: BitVector =
    // todo: this has a log time implementation, assuming a balanced tree
    BitVector(compact.underlying.reverse.map(reverseBitsInByte _))
      .drop(8 - validBitsInLastByte(size))

  /** Returns a new vector of the same size with the byte order reversed.
    *
    * Note that `reverseByteOrder.reverseByteOrder == identity` only when `size` is evenly divisble
    * by 8. To invert `reverseByteOrder` for an arbitrary size, use `invertReverseByteOrder`.
    *
    * @group collection
    */
  final def reverseByteOrder: BitVector =
    if (size % 8 == 0) toBytes(compact.underlying.reverse, size)
    else {
      val validFinalBits = validBitsInLastByte(size)
      val last = take(validFinalBits).compact
      val b = drop(validFinalBits).toByteVector.reverse
      val init = toBytes(b, size - last.size)
      init ++ last
    }

  /** Inverse of `reverseByteOrder`.
    *
    * @group collection
    */
  final def invertReverseByteOrder: BitVector =
    if (size % 8 == 0) reverseByteOrder
    else {
      val validFinalBits = validBitsInLastByte(size)
      val (init, last) = splitAt(size - validFinalBits)
      last ++ init.bytes.reverse.bits
    }

  /** Returns a new vector of the same size with the bit order reversed.
    *
    * @group collection
    */
  final def reverseBitOrder: BitVector = {
    val reversed = compact.underlying.map(reverseBitsInByte _)
    if (size % 8 == 0)
      BitVector(reversed)
    else {
      val lastIdx = reversed.size - 1
      val toDrop = 8 - validBitsInLastByte(size)
      BitVector(reversed.update(lastIdx, (reversed(lastIdx) << toDrop).toByte)).dropRight(toDrop)
    }
  }

  /** Returns the number of bits that are high.
    *
    * @group bitwise
    */
  final def populationCount: Long = {
    @annotation.tailrec
    def go(b: BitVector, acc: Long): Long =
      if (b.isEmpty) acc
      else go(b.tail, if (b.head) acc + 1 else acc)
    go(this, 0)
  }

  final def not: BitVector = mapBytes(_.not)
  final def and(other: BitVector): BitVector = zipBytesWith(other)(_ & _)
  final def or(other: BitVector): BitVector = zipBytesWith(other)(_ | _)
  final def xor(other: BitVector): BitVector = zipBytesWith(other)(_ ^ _)

  final def shiftLeft(n: Long): BitVector =
    if (n <= 0) this
    else if (n >= size) BitVector.low(size)
    else drop(n) ++ BitVector.low(n)

  final def shiftRight(n: Long, signExtension: Boolean): BitVector =
    if (isEmpty || n <= 0) this
    else {
      val extensionHigh = signExtension && head
      if (n >= size)
        if (extensionHigh) BitVector.high(size) else BitVector.low(size)
      else
        (if (extensionHigh) BitVector.high(n) else BitVector.low(n)) ++ dropRight(n)
    }

  final def rotateLeft(n: Long): BitVector =
    if (n <= 0) this
    else if (isEmpty) this
    else {
      val n0 = n % size
      if (n0 == 0) this
      else drop(n0) ++ take(n0)
    }

  final def rotateRight(n: Long): BitVector =
    if (n <= 0) this
    else if (isEmpty) this
    else {
      val n0 = n % size
      if (n0 == 0) this
      else takeRight(n0) ++ dropRight(n0)
    }

  /** Return a `BitVector` with the same contents as `this`, but based off a single `ByteVector`.
    *
    * This may involve copying data to a fresh `ByteVector`, but has the advantage that lookups
    * index directly into a single `ByteVector` rather than traversing a logarithmic number of nodes
    * in this tree.
    *
    * Calling this method on an already compacted vector is a no-op.
    *
    * @group collection
    */
  final def compact: Bytes = {
    if (bytesNeededForBits(size) > Int.MaxValue)
      throw new IllegalArgumentException(
        s"cannot compact bit vector of size ${size.toDouble / 8 / 1e9} GB"
      )

    // we collect up all the chunks, then merge them in O(n * log n)
    @annotation.tailrec
    def go(b: List[BitVector], acc: Vector[Bytes]): Vector[Bytes] =
      b match {
        case (s @ Suspend(_)) :: rem  => go(s.underlying :: rem, acc)
        case (b @ Bytes(_, _)) :: rem => go(rem, acc :+ b)
        case Append(l, r) :: rem      => go(l :: r :: rem, acc)
        case (d: Drop) :: rem         => go(rem, acc :+ d.interpretDrop)
        case (c: Chunks) :: rem       => go(c.chunks.left :: c.chunks.right :: rem, acc)
        case _                        => acc
      }

    this match {
      // common case, we have a single flat `Bytes`, in which case we compact and return it directly
      case bs @ Bytes(b, n) =>
        val b2 = b.compact
        if (b2 eq b) bs
        else Bytes(b2, n)
      // other common case is a drop of a single flat `Bytes`
      case d: Drop =>
        val bs = d.interpretDrop
        val b2 = bs.underlying.compact
        if (b2 eq bs.underlying) bs
        else Bytes(b2, bs.size)
      // otherwise we fall back to general purpose algorithm
      case _ =>
        reduceBalanced(go(List(this), Vector()))(_.size)(_.combine(_)) match {
          case Bytes(b, n) => Bytes(b.compact, n) // we compact the underlying ByteVector as well
        }
    }
  }

  /** Produce a single flat `Bytes` by interpreting any non-byte-aligned appends or drops. Unlike
    * `compact`, the underlying `ByteVector` is not necessarily copied.
    *
    * @group collection
    */
  def align: Bytes

  /** Return a `BitVector` with the same contents as `this`, but based off a single flat
    * `ByteVector`. This function is guaranteed to copy all the bytes in this `BitVector`, unlike
    * `compact`, which may no-op if this `BitVector` already consists of a single `ByteVector`
    * chunk.
    *
    * @group collection
    */
  final def copy: Bytes =
    this match {
      case Bytes(b, n) => Bytes(b.copy, n)
      case _           => this.compact
    }

  /** Forces any `Suspend` nodes in this `BitVector` and ensures the tree is balanced.
    *
    * @group collection
    */
  final def force: BitVector = {
    @annotation.tailrec
    def go(cont: Vector[BitVector]): BitVector =
      if (cont.nonEmpty)
        (cont.head, cont.tail) match {
          case (cur, cont) =>
            cur match {
              case b @ Bytes(_, _) => cont.foldLeft[BitVector](b)(_ ++ _)
              case Append(l, r)    => go(l +: r +: cont)
              case d @ Drop(_, _)  => cont.foldLeft[BitVector](d)(_ ++ _)
              case s @ Suspend(_)  => go(s.underlying +: cont)
              case b: Chunks       => go(b.chunks +: cont)
            }
        }
      else cont.foldLeft(BitVector.empty)(_ ++ _)
    go(Vector(this))
  }

  /** Return the sequence of bits in this vector. The returned `IndexedSeq` is just a view; nothing
    * is actually copied.
    *
    * @throws IllegalArgumentException
    *   if this vector's size exceeds Int.MaxValue
    * @see
    *   acquire
    * @see
    *   toIndexedSeq
    * @group conversions
    */
  final def toIndexedSeq: IndexedSeq[Boolean] =
    intSize
      .map { n =>
        new IndexedSeq[Boolean] {
          def length = n
          def apply(idx: Int): Boolean = BitVector.this.get(idx.toLong)
          override def foldRight[B](z: B)(op: (Boolean, B) => B): B = {
            val it = reverseIterator
            var b = z
            while (it.hasNext)
              b = op(it.next(), b)
            b
          }
        }
      }
      .getOrElse {
        throw new IllegalArgumentException(s"BitVector too big for Seq: $size")
      }

  /** Converts the contents of this vector to a byte vector.
    *
    * If this vector's size does not divide evenly by 8, the last byte of the returned vector will
    * be zero-padded to the right.
    *
    * @group conversions
    */
  final def toByteVector: ByteVector =
    clearUnneededBits(size, compact.underlying)

  /** Alias for [[toByteVector]].
    * @group conversions
    */
  final def bytes: ByteVector = toByteVector

  /** Converts the contents of this vector to a byte array.
    *
    * If this vector's size does not divide evenly by 8, the last byte of the returned vector will
    * be zero-padded to the right.
    *
    * @group conversions
    */
  final def toByteArray: Array[Byte] = toByteVector.toArray

  /** Converts the contents of this vector to a `java.nio.ByteBuffer`.
    *
    * The returned buffer is freshly allocated with limit set to the minimum number of bytes needed
    * to represent the contents of this vector, position set to zero, and remaining set to the
    * limit.
    *
    * @see
    *   toByteVector
    * @group conversions
    */
  final def toByteBuffer: java.nio.ByteBuffer = toByteVector.toByteBuffer

  /** Converts the contents of this bit vector to a binary string of `size` digits.
    *
    * @group conversions
    */
  final def toBin: String = toByteVector.toBin.take(size.toInt)

  /** Converts the contents of this bit vector to a binary string of `size` digits.
    *
    * @group conversions
    */
  final def toBin(alphabet: Bases.BinaryAlphabet): String =
    toByteVector.toBin(alphabet).take(size.toInt)

  /** Converts the contents of this bit vector to a hexadecimal string of `ceil(size / 4)` nibbles.
    *
    * The last nibble is right-padded with zeros if the size is not evenly divisible by 4.
    *
    * @group conversions
    */
  final def toHex: String = toHex(Bases.Alphabets.HexLowercase)

  /** Converts the contents of this bit vector to a hexadecimal string of `ceil(size / 4)` nibbles.
    *
    * The last nibble is right-padded with zeros if the size is not evenly divisible by 4.
    *
    * @group conversions
    */
  final def toHex(alphabet: Bases.HexAlphabet): String = {
    val full = toByteVector.toHex(alphabet)
    size % 8 match {
      case 0           => full
      case n if n <= 4 => full.init
      case _           => full
    }
  }

  /** Generates a hex dump of this vector using the default format (with no color / ANSI escapes).
    * To customize the output, use the `HexDumpFormat` class instead.
    * For example, `HexDumpFormat.NoAscii.render(bytes)` or
    * `HexDumpFormat.Default.withIncludeAddressColumn(false).render(bytes)`.
    *
    * @group conversions
    */
  final def toHexDump: String = HexDumpFormat.NoAnsi.render(this)

  /** Colorized version of [[toHexDump]] that uses ANSI escape codes.
    *
    * @group conversions
    */
  final def toHexDumpColorized: String = HexDumpFormat.Default.render(this)

  /** Prints this vector as a colorized hex dump to standard out.
    */
  final def printHexDump(): Unit = HexDumpFormat.Default.print(this)

  /** Helper alias of [[toHex():String*]]
    *
    * @group conversions
    */
  final def toBase16: String = toHex

  /** Helper alias of [[toHex(alpbabet:scodec\.bits\.Bases\.HexAlphabet):String*]]
    *
    * @group conversions
    */
  final def toBase16(alphabet: Bases.HexAlphabet): String = toHex(alphabet)

  /** Converts the contents of this vector to a base 32 string.
    *
    * The last byte is right-padded with zeros if the size is not evenly divisible by 8.
    *
    * @group conversions
    */
  final def toBase32: String = toBase32(Bases.Alphabets.Base32)

  /** Converts the contents of this vector to a base 32 string using the specified alphabet.
    *
    * The last byte is right-padded with zeros if the size is not evenly divisible by 8.
    *
    * @group conversions
    */
  final def toBase32(alphabet: Bases.Base32Alphabet): String = toByteVector.toBase32(alphabet)

  /** Converts the contents of this vector to a base 58 string.
    *
    * the order is assumed to be the same as
    * (Bitcoin)[[https://en.bitcoin.it/wiki/Base58Check_encoding#Base58_symbol_chart]]
    *
    * @group conversions
    */
  final def toBase58: String = toBase58(Bases.Alphabets.Base58)

  /** Converts the contents of this vector to a base 58 string using the specified alphabet.
    *
    * the order is assumed to be the same as
    * (Bitcoin)[[https://en.bitcoin.it/wiki/Base58Check_encoding#Base58_symbol_chart]]
    *
    * @group conversions
    */
  final def toBase58(alphabet: Bases.Alphabet): String = toByteVector.toBase58(alphabet)

  /** Converts the contents of this vector to a base 64 string.
    *
    * The last byte is right-padded with zeros if the size is not evenly divisible by 8.
    *
    * @group conversions
    */
  final def toBase64: String = toBase64(Bases.Alphabets.Base64)

  /** Converts the contents of this vector to a base 64 string using the specified alphabet.
    *
    * The last byte is right-padded with zeros if the size is not evenly divisible by 8.
    *
    * @group conversions
    */
  final def toBase64(alphabet: Bases.Base64Alphabet): String = toByteVector.toBase64(alphabet)

  /** Converts the contents of this vector to a base 64 string without padding.
    *
    * @group conversions
    */
  final def toBase64NoPad: String = toByteVector.toBase64NoPad

  /** Converts the contents of this vector to a base 64 string without padding.
    *
    * @group conversions
    */
  final def toBase64Url: String = toByteVector.toBase64Url

  /** Converts the contents of this vector to a base 64 string without padding.
    *
    * @group conversions
    */
  final def toBase64UrlNoPad: String = toByteVector.toBase64UrlNoPad

  /** Convert a slice of bits from this vector (`start` until `start+bits`) to a `Byte`.
    *
    * @param signed
    *   whether sign extension should be performed
    * @throws IllegalArgumentException
    *   if the slice refers to indices that are out of range
    * @group conversions
    */
  final def sliceToByte(start: Long, bits: Int, signed: Boolean = true): Byte =
    if (start % 8 != 0) drop(start).sliceToByte(0, bits, signed)
    else if (isEmpty || bits == 0) 0.toByte
    else getByte(start, bits, signed)

  private def getByte(start: Long, bits: Int, signed: Boolean): Byte = {
    require(sizeGreaterThanOrEqual(start + bits) && bits >= 0 && bits <= 8)
    var result = 0x0ff & getByte(start / 8)
    if (bits != 0) result = result >>> (8 - bits)
    // Sign extend if necessary
    if (signed && bits != 8 && ((1 << (bits - 1)) & result) != 0) {
      val toShift = 32 - bits
      result = (result << toShift) >> toShift
    }
    result.toByte
  }

  /** Converts the contents of this vector to a byte.
    *
    * @param signed
    *   whether sign extension should be performed
    * @throws IllegalArgumentException
    *   if size is greater than 8
    * @group conversions
    */
  final def toByte(signed: Boolean = true): Byte = {
    require(sizeLessThanOrEqual(8))
    if (isEmpty) 0.toByte
    else getByte(0, size.toInt, signed)
  }

  /** Convert a slice of bits from this vector (`start` until `start+bits`) to a `Short`.
    *
    * @param signed
    *   whether sign extension should be performed
    * @param ordering
    *   order bytes should be processed in
    * @throws IllegalArgumentException
    *   if the slice refers to indices that are out of range
    * @group conversions
    */
  final def sliceToShort(
      start: Long,
      bits: Int,
      signed: Boolean = true,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): Short =
    if (start % 8 != 0) drop(start).sliceToShort(0, bits, signed, ordering)
    else if (ordering == ByteOrdering.LittleEndian)
      drop(start).invertReverseByteOrder.sliceToShort(0, bits, signed, ByteOrdering.BigEndian)
    else getBigEndianShort(start, bits, signed)

  private def getBigEndianShort(start: Long, bits: Int, signed: Boolean): Short = {
    require(sizeGreaterThanOrEqual(start + bits) && bits >= 0 && bits <= 16)
    val mod = bits % 8
    var result = 0
    val bytesNeeded = bytesNeededForBits(bits.toLong)
    val base = start / 8
    @annotation.tailrec
    def go(i: Int): Unit =
      if (i < bytesNeeded) {
        result = (result << 8) | (0x0ff & this.getByte(base + i))
        go(i + 1)
      }
    go(0)
    if (mod != 0) result = result >>> (8 - mod)
    // Sign extend if necessary
    if (signed && bits != 16 && ((1 << (bits - 1)) & result) != 0) {
      val toShift = 32 - bits
      result = (result << toShift) >> toShift
    }
    result.toShort
  }

  /** Converts the contents of this vector to a short.
    *
    * @param signed
    *   whether sign extension should be performed
    * @param ordering
    *   order bytes should be processed in
    * @throws IllegalArgumentException
    *   if size is greater than 16
    * @group conversions
    */
  final def toShort(
      signed: Boolean = true,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): Short = {
    require(sizeLessThanOrEqual(16))
    if (ordering == ByteOrdering.LittleEndian)
      invertReverseByteOrder.toShort(signed, ByteOrdering.BigEndian)
    else getBigEndianShort(0, size.toInt, signed)
  }

  /** Convert a slice of bits from this vector (`start` until `start+bits`) to an `Int`.
    *
    * @param signed
    *   whether sign extension should be performed
    * @param ordering
    *   order bytes should be processed in
    * @throws IllegalArgumentException
    *   if the slice refers to indices that are out of range
    * @group conversions
    */
  final def sliceToInt(
      start: Long,
      bits: Int,
      signed: Boolean = true,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): Int =
    if (start % 8 != 0) drop(start).sliceToInt(0, bits, signed, ordering)
    else if (ordering == ByteOrdering.LittleEndian)
      drop(start).invertReverseByteOrder.sliceToInt(0, bits, signed, ByteOrdering.BigEndian)
    else getBigEndianInt(start, bits, signed)

  private def getBigEndianInt(start: Long, bits: Int, signed: Boolean): Int = {
    require(sizeGreaterThanOrEqual(start + bits) && bits >= 0 && bits <= 32)
    val mod = bits % 8
    var result = 0
    val bytesNeeded = bytesNeededForBits(bits.toLong)
    val base = start / 8
    @annotation.tailrec
    def go(i: Int): Unit =
      if (i < bytesNeeded) {
        result = (result << 8) | (0x0ff & this.getByte(base + i))
        go(i + 1)
      }
    go(0)
    if (mod != 0) result = result >>> (8 - mod)
    // Sign extend if necessary
    if (signed && bits != 32 && ((1 << (bits - 1)) & result) != 0) {
      val toShift = 32 - bits
      result = (result << toShift) >> toShift
    }
    result
  }

  /** Converts the contents of this vector to an int.
    *
    * @param signed
    *   whether sign extension should be performed
    * @param ordering
    *   order bytes should be processed in
    * @throws IllegalArgumentException
    *   if size is greater than 32
    * @group conversions
    */
  final def toInt(signed: Boolean = true, ordering: ByteOrdering = ByteOrdering.BigEndian): Int = {
    require(sizeLessThanOrEqual(32))
    this match {
      case bytes: Bytes =>
        size.toInt match {
          case 32 if signed =>
            bytes.underlying.toByteBuffer.order(ordering.toJava).getInt

          case 16 =>
            val sh = bytes.underlying.toByteBuffer.order(ordering.toJava).getShort
            if (signed) sh.toInt else sh & 0x0ffff

          case 8 =>
            val b = bytes.underlying.toByteBuffer.get
            if (signed) b.toInt else b & 0x0ff

          case bits =>
            if (ordering == ByteOrdering.LittleEndian)
              invertReverseByteOrder.toInt(signed, ByteOrdering.BigEndian)
            else getBigEndianInt(0, bits, signed)
        }
      case _ =>
        if (ordering == ByteOrdering.LittleEndian)
          invertReverseByteOrder.toInt(signed, ByteOrdering.BigEndian)
        else getBigEndianInt(0, size.toInt, signed)
    }
  }

  /** Convert a slice of bits from this vector (`start` until `start+bits`) to a `Long`.
    *
    * @param signed
    *   whether sign extension should be performed
    * @param ordering
    *   order bytes should be processed in
    * @throws IllegalArgumentException
    *   if the slice refers to indices that are out of range
    * @group conversions
    */
  final def sliceToLong(
      start: Long,
      bits: Int,
      signed: Boolean = true,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): Long =
    if (start % 8 != 0) drop(start).sliceToLong(0, bits, signed, ordering)
    else if (ordering == ByteOrdering.LittleEndian)
      drop(start).invertReverseByteOrder.sliceToLong(0, bits, signed, ByteOrdering.BigEndian)
    else getBigEndianLong(start, bits, signed)

  private def getBigEndianLong(start: Long, bits: Int, signed: Boolean): Long = {
    require(sizeGreaterThanOrEqual(start + bits) && bits >= 0 && bits <= 64)
    val mod = bits % 8
    var result = 0L
    val bytesNeeded = bytesNeededForBits(bits.toLong)
    val base = start / 8
    @annotation.tailrec
    def go(i: Int): Unit =
      if (i < bytesNeeded) {
        result = (result << 8) | (0x0ffL & this.getByte(base + i))
        go(i + 1)
      }
    go(0)
    if (mod != 0) result = result >>> (8 - mod)
    // Sign extend if necessary
    if (signed && bits != 64 && ((1L << (bits - 1)) & result) != 0) {
      val toShift = 64 - bits
      result = (result << toShift) >> toShift
    }
    result
  }

  /** Converts the contents of this vector to a long.
    *
    * @param signed
    *   whether sign extension should be performed
    * @param ordering
    *   order bytes should be processed in
    * @throws IllegalArgumentException
    *   if size is greater than 64
    * @group conversions
    */
  final def toLong(
      signed: Boolean = true,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): Long = {
    require(sizeLessThanOrEqual(64))
    this match {
      case bytes: Bytes =>
        size.toInt match {
          case 64 if signed =>
            bytes.underlying.toByteBuffer.order(ordering.toJava).getLong

          case 32 =>
            val i = bytes.underlying.toByteBuffer.order(ordering.toJava).getInt
            if (signed) i.toLong else i & 0x0ffffffffL

          case 16 =>
            val sh = bytes.underlying.toByteBuffer.order(ordering.toJava).getShort
            if (signed) sh.toLong else sh & 0x0ffffL

          case 8 =>
            val b = bytes.underlying.toByteBuffer.get
            if (signed) b.toLong else b & 0x0ffL

          case bits =>
            if (ordering == ByteOrdering.LittleEndian)
              invertReverseByteOrder.toLong(signed, ByteOrdering.BigEndian)
            else getBigEndianLong(0, bits, signed)
        }

      case _ =>
        if (ordering == ByteOrdering.LittleEndian)
          invertReverseByteOrder.toLong(signed, ByteOrdering.BigEndian)
        else getBigEndianLong(0, size.toInt, signed)
    }
  }

  /** Convert a slice of bits from this vector (`start` until `start+bits`) to a `BigInt`.
    *
    * @param signed
    *   whether sign extension should be performed
    * @param ordering
    *   order bytes should be processed in
    * @throws IllegalArgumentException
    *   if the slice refers to indices that are out of range
    * @group conversions
    */
  final def sliceToBigInt(
      start: Long,
      bits: Int,
      signed: Boolean = true,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): BigInt =
    if (start % 8 != 0) drop(start).sliceToBigInt(0, bits, signed, ordering)
    else if (ordering == ByteOrdering.LittleEndian)
      drop(start).invertReverseByteOrder.sliceToBigInt(0, bits, signed, ByteOrdering.BigEndian)
    else getBigEndianBigInt(start, bits, signed)

  private def getBigEndianBigInt(start: Long, bits: Int, signed: Boolean): BigInt = {
    require(sizeGreaterThanOrEqual(start + bits) && bits >= 0)
    if (bits == 0) BigInt(0)
    else {
      val firstBit = apply(start)
      val signBit = signed && firstBit
      // Include an explicit sign bit of 0 when we're unsigned and the first bit is high
      val explicitZeroSignBit = !signed && firstBit
      val explicitZeroSign = if (explicitZeroSignBit) BitVector.zero else BitVector.empty
      val mod = (bits + explicitZeroSign.size) % 8
      val pad = if (mod == 0) BitVector.empty else BitVector.fill(8 - mod)(signBit)
      val arr = (explicitZeroSign ++ pad ++ slice(start, start + bits)).toByteArray
      BigInt(arr)
    }
  }

  /** Converts the contents of this vector to a `BigInt`.
    *
    * @param signed
    *   whether sign extension should be performed
    * @param ordering
    *   order bytes should be processed in
    * @group conversions
    */
  final def toBigInt(
      signed: Boolean = true,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): BigInt =
    if (ordering == ByteOrdering.LittleEndian)
      invertReverseByteOrder.toBigInt(signed, ByteOrdering.BigEndian)
    else getBigEndianBigInt(0, size.toInt, signed)

  /** Converts the contents of this bit vector to a UUID.
    *
    * @throws IllegalArgumentException
    *   if size is not exactly 128.
    * @group conversions
    */
  final def toUUID: UUID = {
    // Sanity check
    if (size != 128)
      throw new IllegalArgumentException(
        s"Cannot convert BitVector of size $size to UUID; must be 128 bits"
      )
    // Convert
    val byteBuffer = toByteBuffer
    val mostSignificant = byteBuffer.getLong
    val leastSignificant = byteBuffer.getLong
    new UUID(mostSignificant, leastSignificant)
  }

  /** Decodes this vector as a string using the implicitly available charset.
    * @group conversions
    */
  final def decodeString(implicit charset: Charset): Either[CharacterCodingException, String] =
    bytes.decodeString(charset)

  /** Decodes this vector as a string using the UTF-8 charset.
    * @group conversions
    */
  final def decodeUtf8: Either[CharacterCodingException, String] =
    bytes.decodeUtf8

  /** Decodes this vector as a string using the US-ASCII charset.
    * @group conversions
    */
  final def decodeAscii: Either[CharacterCodingException, String] =
    bytes.decodeAscii

  /** Returns true if the specified `BitVector` has the same contents as this vector.
    * @group collection
    */
  final def ===(other: BitVector): Boolean =
    if (this.eq(other)) true
    else if (this.length != other.length) false
    else {
      val chunkSize = 8L * 1024 * 64
      @annotation.tailrec
      def go(x: BitVector, y: BitVector): Boolean =
        if (x.isEmpty) y.isEmpty
        else {
          val chunkX = x.take(chunkSize)
          val chunkY = y.take(chunkSize)
          chunkX.toByteVector === chunkY.toByteVector &&
          go(x.drop(chunkSize), y.drop(chunkSize))
        }

      go(this, other)
    }

  /** Returns true if the specified value is a `BitVector` with the same contents as this vector.
    * @see
    *   [[BitVector.===]]
    * @group collection
    */
  override final def equals(other: Any): Boolean =
    other match {
      case o: BitVector => this === o
      case _            => false
    }

  /** Calculates the hash code of this vector. The result is cached.
    * @group collection
    */
  override final lazy val hashCode = {
    // todo: this could be recomputed more efficiently using the tree structure
    // given an associative hash function
    import util.hashing.MurmurHash3._
    val chunkSize = 8L * 1024 * 64
    @annotation.tailrec
    def go(bits: BitVector, h: Int, iter: Int): Int =
      if (bits.isEmpty) finalizeHash(h, iter)
      else go(bits.drop(chunkSize), mix(h, bytesHash(bits.take(chunkSize).toByteArray)), iter + 1)
    go(this, stringHash("BitVector"), 1)
  }

  /** Display the size and bytes of this `BitVector`. For bit vectors beyond a certain size, only a
    * hash of the contents are shown.
    * @group collection
    */
  override final def toString =
    if (isEmpty) "BitVector(empty)"
    else if (sizeLessThan(513)) s"BitVector($size bits, 0x$toHex)"
    else s"BitVector($size bits, #$hashCode)"

  // impl details

  final protected def checkBounds(n: Long): Unit =
    if (!sizeGreaterThan(n)) outOfBounds(n)

  final protected def outOfBounds(n: Long): Nothing =
    throw new NoSuchElementException(s"invalid index: $n of $size")

  final protected def mapBytes(f: ByteVector => ByteVector): BitVector =
    this match {
      case Bytes(bs, n)   => toBytes(f(bs), n)
      case Append(l, r)   => Append(l.mapBytes(f), r.mapBytes(f))
      case Drop(b, n)     => Drop(b.mapBytes(f).compact, n)
      case s @ Suspend(_) => Suspend(() => s.underlying.mapBytes(f))
      case c: Chunks      => Chunks(Append(c.chunks.left.mapBytes(f), c.chunks.right.mapBytes(f)))
    }

  /** Pretty print this `BitVector`.
    */
  private[bits] def internalPretty(prefix: String): String =
    this match {
      case Append(l, r) =>
        prefix + "append\n" +
          l.internalPretty(prefix + "  ") + "\n" +
          r.internalPretty(prefix + "  ")
      case Bytes(b, n) => prefix + s"bits $n\n" + b.pretty("  " + prefix)
      case Drop(u, n)  =>
        prefix + s"drop ${n}\n" +
          u.internalPretty(prefix + "  ")
      case s @ Suspend(_) => prefix + "suspend\n" + s.underlying.internalPretty(prefix + "  ")
      case c: Chunks      =>
        prefix + "chunks\n" +
          c.chunks.left.internalPretty("  ") + "\n" +
          c.chunks.right.internalPretty("  ")
    }

  private def zipBytesWith(other: BitVector)(op: (Byte, Byte) => Int): BitVector =
    // todo: this has a much more efficient recursive algorithm -
    // only need to compact close to leaves of the tree
    toBytes(
      this.compact.underlying.zipWithI(other.compact.underlying)(op),
      this.size.min(other.size)
    )

  protected final def writeReplace(): AnyRef = new SerializationProxy(toByteArray, size)

  override def compare(that: BitVector): Int =
    if (this.eq(that))
      0
    else {
      val thisLength = this.length
      val thatLength = that.length
      val commonLength = thisLength.min(thatLength)
      var i = 0
      while (i < commonLength) {
        val cmp = this(i.toLong).compare(that(i.toLong))
        if (cmp != 0)
          return cmp
        i = i + 1
      }
      if (thisLength < thatLength)
        -1
      else if (thisLength > thatLength)
        1
      else
        0
    }
}

/** Companion for [[BitVector]].
  *
  * @groupname constants
  *   Constants
  * @groupprio constants
  *   0
  *
  * @groupname constructors
  *   Constructors
  * @groupprio constructors
  *   1
  *
  * @groupname numeric
  *   Numeric Conversions
  * @groupprio numeric
  *   2
  *
  * @groupname base
  *   Base Conversions
  * @groupprio base
  *   3
  */
object BitVector extends BitVectorCompanionCrossPlatform {

  /** Empty bit vector.
    * @group constants
    */
  val empty: BitVector = toBytes(ByteVector.empty, 0)

  /** 1-bit vector with only bit set low.
    * @group constants
    */
  val zero: BitVector = toBytes(ByteVector(0), 1)

  /** 1-bit vector with only bit set high.
    * @group constants
    */
  val one: BitVector = toBytes(ByteVector(0xff), 1)

  /** 8-bit vector with all bits set low.
    * @group constants
    */
  val lowByte: BitVector = toBytes(ByteVector.low(1), 8)

  /** 8-bit vector with all bits set high.
    * @group constants
    */
  val highByte: BitVector = toBytes(ByteVector.high(1), 8)

  /** 1-bit vector with only bit set to specified value.
    * @group constructors
    */
  def bit(high: Boolean): BitVector = if (high) one else zero

  /** n-bit vector with bit at index `i` set to value of boolean at index `i` in specified iterable.
    * @group constructors
    */
  def bits(b: Iterable[Boolean]): BitVector =
    b.iterator.zipWithIndex.foldLeft(low(b.size.toLong))((acc, b) => acc.update(b._2.toLong, b._1))

  /** n-bit vector with all bits set high.
    * @group constructors
    */
  def high(n: Long): BitVector = fill(n)(true)

  /** n-bit vector with all bits set low.
    * @group constructors
    */
  def low(n: Long): BitVector = fill(n)(false)

  /** Constructs a `BitVector` from a `ByteVector`. This method has runtime O(1).
    * @group constructors
    */
  def apply(bs: ByteVector): BitVector = toBytes(bs, bs.size.toLong * 8)

  /** Constructs a `BitVector` from a `ByteBuffer`. The given `ByteBuffer` is is copied to ensure
    * the resulting `BitVector` is immutable. If this is not desired, use `BitVector.view`.
    * @group constructors
    */
  def apply(buffer: ByteBuffer): BitVector = apply(ByteVector(buffer))

  /** Constructs a `BitVector` from an `Array[Byte]`. The given `Array[Byte]` is is copied to ensure
    * the resulting `BitVector` is immutable. If this is not desired, use `BitVector.view`.
    * @group constructors
    */
  def apply(bs: Array[Byte]): BitVector = toBytes(ByteVector(bs), bs.size.toLong * 8)

  /** Constructs a `BitVector` from a collection of bytes.
    * @group constructors
    */
  def apply(bs: IterableOnce[Byte]): BitVector = apply(ByteVector(bs))

  /** Constructs a `BitVector` from a list of literal bytes. Only the least significant byte is used
    * of each integral value.
    * @group constructors
    */
  def apply[A: Integral](bytes: A*): BitVector = apply(ByteVector(bytes: _*))

  /** Constructs a `BitVector` from a `ByteBuffer` using the buffer limit * 8 as the size. Unlike
    * `apply`, this does not make a copy of the input buffer, so callers should take care not to
    * modify the contents of the buffer passed to this function.
    * @group constructors
    */
  def view(buffer: ByteBuffer): BitVector =
    toBytes(ByteVector.view(buffer), buffer.limit().toLong * 8)

  /** Constructs a `BitVector` from the first `sizeInBits` of the `ByteBuffer`. Unlike `apply`, this
    * does not make a copy of the input buffer, so callers should take care not to modify the
    * contents of the buffer passed to this function.
    * @group constructors
    */
  def view(buffer: ByteBuffer, sizeInBits: Long): BitVector =
    toBytes(ByteVector.view(buffer), sizeInBits)

  /** Constructs a `BitVector` from an `Array[Byte]`. Unlike `apply`, this does not make a copy of
    * the input array, so callers should take care not to modify the contents of the array passed to
    * this function.
    * @group constructors
    */
  def view(bs: Array[Byte]): BitVector = view(bs, bs.size.toLong * 8)

  /** Constructs a `BitVector` from an `Array[Byte]`. Unlike `apply`, this does not make a copy of
    * the input array, so callers should take care not to modify the contents of the array passed to
    * this function.
    * @group constructors
    */
  def view(bs: Array[Byte], sizeInBits: Long): BitVector = toBytes(ByteVector.view(bs), sizeInBits)

  /** Constructs an `n`-bit `BitVector` where each bit is set to the specified value.
    * @group constructors
    */
  def fill(n: Long)(high: Boolean): BitVector = {
    val needed = bytesNeededForBits(n)
    val bs = ByteVector.fill(needed)(if (high) -1 else 0)
    toBytes(bs, n)
  }

  /** Constructs a bit vector with the 2's complement encoding of the specified byte.
    * @param b
    *   value to encode
    * @param size
    *   size of vector (<= 8)
    * @group numeric
    */
  def fromByte(b: Byte, size: Int = 8): BitVector = {
    require(size <= 8)
    (BitVector(b) << (8L - size)).take(size.toLong)
  }

  /** Constructs a bit vector with the 2's complement encoding of the specified value.
    * @param s
    *   value to encode
    * @param size
    *   size of vector (<= 16)
    * @param ordering
    *   byte ordering of vector
    * @group numeric
    */
  def fromShort(
      s: Short,
      size: Int = 16,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): BitVector = {
    require(size <= 16)
    val buffer = ByteBuffer.allocate(2).order(ByteOrder.BIG_ENDIAN).putShort(s)
    buffer.flip()
    val relevantBits = (BitVector.view(buffer) << (16L - size)).take(size.toLong)
    if (ordering == ByteOrdering.BigEndian) relevantBits else relevantBits.reverseByteOrder
  }

  /** Constructs a bit vector with the 2's complement encoding of the specified value.
    * @param i
    *   value to encode
    * @param size
    *   size of vector (<= 32)
    * @param ordering
    *   byte ordering of vector
    * @group numeric
    */
  def fromInt(
      i: Int,
      size: Int = 32,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): BitVector = {
    require(size <= 32)
    val buffer = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(i)
    buffer.flip()
    val relevantBits = (BitVector.view(buffer) << (32L - size)).take(size.toLong)
    if (ordering == ByteOrdering.BigEndian) relevantBits else relevantBits.reverseByteOrder
  }

  /** Constructs a bit vector with the 2's complement encoding of the specified value.
    * @param l
    *   value to encode
    * @param size
    *   size of vector (<= 64)
    * @param ordering
    *   byte ordering of vector
    * @group numeric
    */
  def fromLong(
      l: Long,
      size: Int = 64,
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): BitVector = {
    require(size <= 64)
    val buffer = ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putLong(l)
    buffer.flip()
    val relevantBits = (BitVector.view(buffer) << (64L - size)).take(size.toLong)
    if (ordering == ByteOrdering.BigEndian) relevantBits else relevantBits.reverseByteOrder
  }

  /** Constructs a bit vector with the 2's complement encoding of the specified value.
    * @param value
    *   value to encode
    * @param size
    *   size of vector; if undefined, the minimum number of bits are used
    * @param ordering
    *   byte ordering of vector
    * @group numeric
    */
  def fromBigInt(
      value: BigInt,
      size: Option[Int],
      ordering: ByteOrdering = ByteOrdering.BigEndian
  ): BitVector = {
    val actualSize = size.getOrElse(value.bitLength + 1)
    require(actualSize > 0)
    val bits = BitVector.view(value.toByteArray)
    val relevantBits =
      if (bits.size < actualSize) BitVector.fill(actualSize - bits.size)(bits.head) ++ bits
      else bits.takeRight(actualSize)
    if (ordering == ByteOrdering.BigEndian) relevantBits else relevantBits.reverseByteOrder
  }

  /** Constructs a bit vector containing the binary representation of the specified UUID. The bits
    * are in MSB-to-LSB order.
    *
    * @param u
    *   value to encode
    * @group conversions
    */
  final def fromUUID(u: UUID): BitVector = {
    val buf = ByteBuffer.allocate(16)
    buf.putLong(u.getMostSignificantBits)
    buf.putLong(u.getLeastSignificantBits)
    // Go via Array[Byte] to avoid hanging on to intermediate ByteBuffer via AtByteBuffer.
    view(buf.array())
  }

  /** Constructs a `BitVector` from a binary string or returns an error message if the string is not
    * valid binary.
    *
    * The string may start with a `0b` and it may contain whitespace or underscore characters.
    * @group base
    */
  def fromBinDescriptive(
      str: String,
      alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary
  ): Either[String, BitVector] =
    try {
      val (bytes, count) = ByteVector.fromBinInternal(str, alphabet)
      val toDrop = count match {
        case 0               => 0
        case n if n % 8 == 0 => 0
        case n               => 8 - (n % 8)
      }
      Right(bytes.toBitVector.drop(toDrop.toLong))
    } catch {
      case t: IllegalArgumentException => Left(t.getMessage)
    }

  /** Constructs a `BitVector` from a binary string or returns `None` if the string is not valid
    * binary.
    *
    * The string may start with a `0b` and it may contain whitespace or underscore characters.
    * @group base
    */
  def fromBin(
      str: String,
      alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary
  ): Option[BitVector] = fromBinDescriptive(str, alphabet).toOption

  /** Constructs a `BitVector` from a binary string or throws an IllegalArgumentException if the
    * string is not valid binary.
    *
    * The string may start with a `0b` and it may contain whitespace or underscore characters.
    *
    * @throws IllegalArgumentException
    *   if the string is not valid hexadecimal
    * @group base
    */
  def fromValidBin(
      str: String,
      alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary
  ): BitVector =
    fromBinDescriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)

  /** Constructs a `BitVector` from a hexadecimal string or returns an error message if the string
    * is not valid hexadecimal.
    *
    * The string may start with a `0x` and it may contain whitespace or underscore characters.
    * @group base
    */
  def fromHexDescriptive(
      str: String,
      alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase
  ): Either[String, BitVector] =
    try {
      val (bytes, count) = ByteVector.fromHexInternal(str, alphabet)
      val toDrop = if (count % 2 == 0) 0 else 4
      Right(bytes.toBitVector.drop(toDrop.toLong))
    } catch {
      case t: IllegalArgumentException => Left(t.getMessage)
    }

  /** Constructs a `BitVector` from a hexadecimal string or returns `None` if the string is not
    * valid hexadecimal.
    *
    * The string may start with a `0x` and it may contain whitespace or underscore characters.
    * @group base
    */
  def fromHex(
      str: String,
      alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase
  ): Option[BitVector] = fromHexDescriptive(str, alphabet).toOption

  /** Constructs a `BitVector` from a hexadecimal string or throws an IllegalArgumentException if
    * the string is not valid hexadecimal.
    *
    * The string may start with a `0x` and it may contain whitespace or underscore characters.
    *
    * @throws IllegalArgumentException
    *   if the string is not valid hexadecimal
    * @group base
    */
  def fromValidHex(
      str: String,
      alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase
  ): BitVector =
    fromHexDescriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)

  /** Constructs a `BitVector` from a base 32 string or returns an error message if the string is
    * not valid base 32. Details pertaining to base 32 decoding can be found in the comment for
    * ByteVector.fromBase32Descriptive. The string may contain whitespace characters which are
    * ignored.
    * @group base
    */
  def fromBase32Descriptive(
      str: String,
      alphabet: Bases.Base32Alphabet = Bases.Alphabets.Base32
  ): Either[String, BitVector] =
    ByteVector.fromBase32Descriptive(str, alphabet).map(_.toBitVector)

  /** Constructs a `BitVector` from a base 32 string or returns `None` if the string is not valid
    * base 32. Details pertaining to base 32 decoding can be found in the comment for
    * ByteVector.fromBase32Descriptive. The string may contain whitespace characters which are
    * ignored.
    * @group base
    */
  def fromBase32(
      str: String,
      alphabet: Bases.Base32Alphabet = Bases.Alphabets.Base32
  ): Option[BitVector] = fromBase32Descriptive(str, alphabet).toOption

  /** Constructs a `BitVector` from a base 32 string or throws an IllegalArgumentException if the
    * string is not valid base 32. Details pertaining to base 32 decoding can be found in the
    * comment for ByteVector.fromBase32Descriptive. The string may contain whitespace characters
    * which are ignored.
    *
    * @throws IllegalArgumentException
    *   if the string is not valid base 32
    * @group base
    */
  def fromValidBase32(
      str: String,
      alphabet: Bases.Base32Alphabet = Bases.Alphabets.Base32
  ): BitVector =
    fromBase32Descriptive(str, alphabet)
      .fold(msg => throw new IllegalArgumentException(msg), identity)

  /** Constructs a `BitVector` from a base 58 string or returns an error message if the string is
    * not valid base 58. Details pertaining to base 58 decoding can be found in the comment for
    * ByteVector.fromBase58Descriptive. The string may contain whitespace characters which are
    * ignored.
    * @group base
    */
  def fromBase58Descriptive(
      str: String,
      alphabet: Bases.Alphabet = Bases.Alphabets.Base58
  ): Either[String, BitVector] =
    ByteVector.fromBase58Descriptive(str, alphabet).map(_.toBitVector)

  /** Constructs a `BitVector` from a base 58 string or returns `None` if the string is not valid
    * base 58. Details pertaining to base 58 decoding can be found in the comment for
    * ByteVector.fromBase58Descriptive. The string may contain whitespace characters which are
    * ignored.
    * @group base
    */
  def fromBase58(
      str: String,
      alphabet: Bases.Alphabet = Bases.Alphabets.Base58
  ): Option[BitVector] = fromBase58Descriptive(str, alphabet).toOption

  /** Constructs a `BitVector` from a base 58 string or throws an IllegalArgumentException if the
    * string is not valid base 58. Details pertaining to base 58 decoding can be found in the
    * comment for ByteVector.fromBase58Descriptive. The string may contain whitespace characters
    * which are ignored.
    *
    * @throws IllegalArgumentException
    *   if the string is not valid base 58
    * @group base
    */
  def fromValidBase58(str: String, alphabet: Bases.Alphabet = Bases.Alphabets.Base58): BitVector =
    fromBase58Descriptive(str, alphabet)
      .fold(msg => throw new IllegalArgumentException(msg), identity)

  /** Constructs a `BitVector` from a base 64 string or returns an error message if the string is
    * not valid base 64. Details pertaining to base 64 decoding can be found in the comment for
    * ByteVector.fromBase64Descriptive. The string may contain whitespace characters which are
    * ignored.
    * @group base
    */
  def fromBase64Descriptive(
      str: String,
      alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64
  ): Either[String, BitVector] =
    ByteVector.fromBase64Descriptive(str, alphabet).map(_.toBitVector)

  /** Constructs a `BitVector` from a base 64 string or returns `None` if the string is not valid
    * base 64. Details pertaining to base 64 decoding can be found in the comment for
    * ByteVector.fromBase64Descriptive. The string may contain whitespace characters which are
    * ignored.
    * @group base
    */
  def fromBase64(
      str: String,
      alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64
  ): Option[BitVector] = fromBase64Descriptive(str, alphabet).toOption

  /** Constructs a `BitVector` from a base 64 string or throws an IllegalArgumentException if the
    * string is not valid base 64. Details pertaining to base 64 decoding can be found in the
    * comment for ByteVector.fromBase64Descriptive. The string may contain whitespace characters
    * which are ignored.
    *
    * @throws IllegalArgumentException
    *   if the string is not valid base 64
    * @group base
    */
  def fromValidBase64(
      str: String,
      alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64
  ): BitVector =
    fromBase64Descriptive(str, alphabet)
      .fold(msg => throw new IllegalArgumentException(msg), identity)

  /** Encodes the specified string to a `BitVector` using the implicitly available `Charset`.
    *
    * @group constructors
    */
  def encodeString(
      str: String
  )(implicit charset: Charset): Either[CharacterCodingException, BitVector] =
    ByteVector.encodeString(str)(charset).map(_.bits)

  /** Encodes the specified string to a `BitVector` using the UTF-8 charset.
    *
    * @group constructors
    */
  def encodeUtf8(str: String): Either[CharacterCodingException, BitVector] =
    ByteVector.encodeUtf8(str).map(_.bits)

  /** Encodes the specified string to a `BitVector` using the US-ASCII charset.
    *
    * @group constructors
    */
  def encodeAscii(str: String): Either[CharacterCodingException, BitVector] =
    ByteVector.encodeAscii(str).map(_.bits)

  /** Concatenates all the given `BitVector`s into a single instance.
    *
    * @group constructors
    */
  def concat(bvs: IterableOnce[BitVector]): BitVector =
    bvs.iterator.foldLeft(BitVector.empty)(_ ++ _)

  /** Create a lazy `BitVector` by repeatedly extracting chunks from `S`. The returned `BitVector`
    * will have the structure of a fully lazy right-associated cons list. Thus, `get`, `take`, and
    * `drop` will be efficient when operating on the head of the list, but accessing later indices
    * (for `takeRight`, say, or `get(size-1)` will require forcing the stream up to that point.
    *
    * Use `force` if you wish to convert the result to an in-memory strict `BitVector` backed by a
    * balanced tree.
    *
    * @group constructors
    */
  def unfold[S](s: S)(f: S => Option[(BitVector, S)]): BitVector =
    Suspend { () =>
      f(s)
        .map { case (h, t) => Append(h, unfold(t)(f)) }
        .getOrElse(BitVector.empty)
    }

  /** Produce a lazy `BitVector` from the given `InputStream`, using `chunkSizeInBytes` to control
    * the number of bytes read in each chunk (defaulting to 16MB). This simply calls
    * [[scodec.bits.BitVector.unfold]] with a function to extract a series of flat byte arrays from
    * the `InputStream`.
    *
    * This function does not handle closing the `InputStream` and has all the usual drawbacks of
    * lazy I/O - `I/O` exceptions may be raised unexpectedly in pure code as chunks are forced, and
    * it must memoize the results to prevent the underlying side effects from being observed.
    * Streaming applications should take care to ensure that the head of the stream is not left on
    * the stack, as this will cause the entire stream to be retained in memory.
    *
    * @param chunkSizeInBytes
    *   the number of bytes to read in each chunk
    * @group constructors
    */
  def fromInputStream(
      in: java.io.InputStream,
      chunkSizeInBytes: Int = 1024 * 1000 * 16
  ): BitVector =
    unfold(in) { in =>
      val buf = new Array[Byte](chunkSizeInBytes)
      val nRead = in.read(buf)
      if (nRead == chunkSizeInBytes) Some((BitVector(buf), in))
      else if (nRead == -1) None
      else Some((BitVector(buf.take(nRead): Array[Byte]), in))
    }

  /** Produce a lazy `BitVector` from the given `ReadableByteChannel`, using `chunkSizeInBytes` to
    * control the number of bytes read in each chunk (defaulting to 16MB). This function does lazy
    * I/O, see [[scodec.bits.BitVector.fromInputStream]] for caveats. The `direct` parameter, if
    * `true`, allows for (but does not enforce) using a 'direct' `java.nio.ByteBuffer` for each
    * chunk, which means the buffer and corresponding `BitVector` chunk may be backed by a 'view'
    * rather than an in-memory array. This may be more efficient for some workloads. See
    * `java.nio.ByteBuffer` for more information.
    *
    * @param chunkSizeInBytes
    *   the number of bytes to read in each chunk
    * @param direct
    *   true if we should attempt to use a 'direct' `java.nio.ByteBuffer` for reads
    * @group constructors
    */
  def fromChannel(
      in: java.nio.channels.ReadableByteChannel,
      chunkSizeInBytes: Int = 1024 * 1000 * 16,
      direct: Boolean = false
  ): BitVector =
    unfold(in) { in =>
      val buf =
        if (direct) java.nio.ByteBuffer.allocateDirect(chunkSizeInBytes)
        else java.nio.ByteBuffer.allocate(chunkSizeInBytes)
      val nRead = in.read(buf)
      buf.flip
      if (nRead != -1) Some((BitVector.view(buf, nRead.toLong * 8), in))
      else None
    }

  /** Produce a lazy `BitVector` from the given `FileChannel`, using `chunkSizeInBytes` to control
    * the number of bytes read in each chunk (defaulting to 16MB). Unlike
    * [[scodec.bits.BitVector.fromChannel]], this memory-maps chunks in, rather than copying them
    * explicitly.
    *
    * Behavior is unspecified if this function is used concurrently with the underlying file being
    * written.
    *
    * @param chunkSizeInBytes
    *   the number of bytes to read in each chunk
    * @group constructors
    */
  def fromMmap(
      in: java.nio.channels.FileChannel,
      chunkSizeInBytes: Int = 1024 * 1000 * 16
  ): BitVector =
    unfold(in -> 0L) { case (in, pos) =>
      if (pos == in.size) None
      else {
        require(pos < in.size)
        val bytesToRead = (in.size - pos).min(chunkSizeInBytes.toLong)
        val buf = in.map(java.nio.channels.FileChannel.MapMode.READ_ONLY, pos, bytesToRead)
        require(buf.limit() == bytesToRead)
        Some((BitVector.view(buf), in -> (pos + bytesToRead)))
      }
    }

  /** Smart constructor for `Bytes`. */
  private[scodec] def toBytes(bs: ByteVector, sizeInBits: Long): Bytes = {
    val needed = bytesNeededForBits(sizeInBits)
    require(needed <= bs.size)
    val b = if (bs.size > needed) bs.take(needed) else bs
    Bytes(b, sizeInBits)
  }

  private[scodec] case class Bytes(val underlying: ByteVector, val size: Long) extends BitVector {
    private def invalidBits = 8 - validBitsInLastByte(size)
    def align = this
    def sizeLessThan(n: Long) = size < n
    def take(n: Long): Bytes = toBytes(underlying, math.max(0L, math.min(size, n)))
    def drop(n: Long): BitVector =
      if (n >= size) BitVector.empty
      else if (n <= 0) this
      else if (n % 8 == 0) Bytes(underlying.drop(n / 8), size - n)
      else Drop(this, n)
    def get(n: Long): Boolean = {
      checkBounds(n)
      getBit(underlying(n / 8), (n % 8).toInt)
    }
    def getByte(n: Long): Byte =
      if (n < underlying.size - 1)
        underlying(n)
      else { // last byte may have some garbage bits, clear these out
        val valid = 8 - invalidBits
        (underlying(n) & topNBits(valid.toInt)).toByte
      }

    def update(n: Long, high: Boolean): BitVector = {
      checkBounds(n)
      val b2 = underlying.update(
        n / 8,
        underlying.lift(n / 8).map(setBit(_, (n % 8).toInt, high)).getOrElse {
          outOfBounds(n)
        }
      )
      Bytes(b2, size)
    }
    def combine(other: Bytes): Bytes = {
      val invalidBits = this.invalidBits
      val otherBytes = other.underlying
      if (isEmpty)
        other
      else if (otherBytes.isEmpty)
        this
      else if (invalidBits == 0)
        toBytes(underlying ++ otherBytes, size + other.size)
      else {
        val bytesCleared = clearUnneededBits(size, underlying) // this is key
        val hi = bytesCleared(bytesCleared.size - 1)
        val lo =
          (((otherBytes.head & topNBits(invalidBits.toInt)) & 0x000000ff) >>> validBitsInLastByte(
            size
          ).toInt).toByte
        val updatedOurBytes = bytesCleared.update(bytesCleared.size - 1, (hi | lo).toByte)
        val updatedOtherBytes = other.drop(invalidBits).toByteVector
        toBytes(updatedOurBytes ++ updatedOtherBytes, size + other.size)
      }
    }
  }

  private[scodec] case class Drop(underlying: Bytes, m: Long) extends BitVector {
    def size = math.max(0, underlying.size - m)
    def sizeLessThan(n: Long) = size < n
    def align = interpretDrop

    def drop(n: Long): BitVector =
      if (n >= size) BitVector.empty
      else if (n <= 0) this
      else {
        val nm = n + m
        val d = Drop(underlying, nm)
        if (nm > 32768 && nm % 8 == 0) d.interpretDrop // occasionally
        else d
      }
    def take(n: Long): BitVector =
      if (n >= size) this
      else if (n <= 0) BitVector.empty
      else underlying.take(m + n).drop(m)

    def get(n: Long): Boolean =
      underlying.get(m + n)
    def getByte(n: Long): Byte =
      this.drop(n * 8).take(8).align.getByte(0)
    def update(n: Long, high: Boolean): BitVector =
      Drop(underlying.update(m + n, high).compact, m)

    def interpretDrop: Bytes = {
      val low = m.max(0)
      val newSize = size
      if (newSize == 0) BitVector.empty.align
      else {
        val lowByte = low / 8
        val shiftedByWholeBytes: ByteVector =
          underlying.underlying.slice(lowByte, lowByte + bytesNeededForBits(newSize) + 1)
        val bitsToShiftEachByte = (low % 8).toInt
        val newBytes =
          if (bitsToShiftEachByte == 0) shiftedByWholeBytes
          else
            shiftedByWholeBytes.zipWithI(shiftedByWholeBytes.drop(1) :+ (0: Byte)) { case (a, b) =>
              val hi = a << bitsToShiftEachByte
              val low =
                ((b & topNBits(bitsToShiftEachByte)) & 0x000000ff) >>> (8 - bitsToShiftEachByte)
              hi | low
            }
        toBytes(
          if (newSize <= (newBytes.size - 1) * 8) newBytes.dropRight(1) else newBytes,
          newSize
        )
      }
    }
  }
  private[scodec] case class Append(left: BitVector, right: BitVector) extends BitVector {

    def get(n: Long): Boolean =
      if (n < left.size) left.get(n)
      else right.get(n - left.size)
    def getByte(n: Long): Byte =
      if (n < left.size / 8) left.getByte(n)
      else if (left.size % 8 == 0 && n > left.size / 8) right.getByte(n - left.size / 8)
      else
        drop(n * 8).take(8).align.getByte(0) // fall back to inefficient impl (todo: improve this)
    def update(n: Long, high: Boolean): BitVector =
      if (n < left.size) Append(left.update(n, high), right)
      else Append(left, right.update(n - left.size, high))
    def align = left.align.combine(right.align)

    @volatile var knownSize: Long = right match {
      case _: Suspend => -1L
      case _          => // eagerly compute the size if we're strict
        val sz = left.size + right.size
        sz
    }
    var sizeLowerBound = left.size

    def size =
      if (knownSize != -1L) knownSize
      else { // faster to just allow recomputation if there's contention
        @annotation.tailrec
        def go(rem: List[BitVector], acc: Long): Long =
          rem match {
            case Nil                       => acc
            case Append(x, y) :: t         => go(x :: y :: t, acc)
            case Chunks(Append(x, y)) :: t => go(x :: y :: t, acc)
            case (s: Suspend) :: t         => go(s.underlying :: t, acc)
            case h :: t                    => go(t, acc + h.size)
          }
        val sz = go(List(left, right), 0)
        knownSize = sz
        sz
      }

    def take(n: Long) = {
      // NB: not worth early termination in event that sizeLessThanOrEqual(n) is true -
      // this case is rare, and requires traversing same BitVector twice, once to compute
      // sizeLessThan, then again to implement the `take`
      val npos = math.max(0L, n)
      if (npos == 0) BitVector.empty
      else if (npos <= left.size) left.take(npos)
      else {
        @annotation.tailrec
        def go(accL: BitVector, cur: BitVector, n: Long): BitVector =
          cur match {
            case Append(left, right) =>
              if (n <= left.size) accL ++ left.take(n)
              else go(accL ++ left, right, n - left.size)
            case s: Suspend => go(accL, s.underlying, n)
            case _          => accL ++ cur.take(n)
          }
        go(left, right, npos - left.size)
      }
    }

    def drop(n: Long) = {
      val npos = math.max(0L, n)
      if (npos == 0) this
      else {
        @annotation.tailrec
        def go(cur: BitVector, n: Long): BitVector =
          cur match {
            case Append(left, right) =>
              if (n >= left.size) go(right, n - left.size)
              else Append(left.drop(n), right)
            case s: Suspend => go(s.underlying, n)
            case _          => cur.drop(n)
          }
        if (npos >= left.size) go(right, npos - left.size)
        else Append(left.drop(npos), right)
      }
    }

    def sizeLessThan(n: Long) =
      if (knownSize != -1L) knownSize < n
      else if (sizeLowerBound >= n) false
      else {
        @annotation.tailrec
        def go(cur: BitVector, n: Long, seen: Long): Boolean =
          cur match {
            case Append(l, r) =>
              if (l.size >= n) {
                sizeLowerBound = math.max(seen + l.size, sizeLowerBound); false
              } else go(r, n - l.size, seen + l.size)
            case s: Suspend => go(s.underlying, n, seen)
            case _          =>
              sizeLowerBound = math.max(seen, sizeLowerBound)
              cur.size < n
          }
        go(this, n, 0)
      }
  }
  private[scodec] case class Suspend(thunk: () => BitVector) extends BitVector {
    lazy val underlying = thunk()
    def sizeLessThan(n: Long) = underlying.sizeLessThan(n)
    def get(n: Long): Boolean = underlying.get(n)
    def take(n: Long) = underlying.take(n)
    def drop(n: Long) = underlying.drop(n)
    def getByte(n: Long): Byte = underlying.getByte(n)
    def update(n: Long, high: Boolean): BitVector = underlying.update(n, high)
    def size = underlying.size
    def align = underlying.align
  }

  /** A vector of chunks of exponentially decreasing size. Supports amortized constant time `++` and
    * logarithmic time for all other operations.
    */
  private[scodec] case class Chunks(chunks: Append) extends BitVector {

    override def unchunk = Append(chunks.left, chunks.right.unchunk)
    def align = chunks.align

    def take(n: Long) = chunks.take(n)
    def drop(n: Long) = chunks.drop(n)

    override def ++(b: BitVector): BitVector =
      if (b.isEmpty) this
      else if (this.isEmpty) b
      else {
        @annotation.tailrec
        def go(chunks: Append, last: BitVector): BitVector = {
          val lastN = last.size
          if (lastN >= chunks.size || lastN * 2 <= chunks.right.size)
            Chunks(Append(chunks, last))
          else
            chunks.left match {
              case left: Append =>
                val rN = chunks.right.size
                val aligned = (lastN % 8) + (rN % 8) == 0
                if (rN <= 256 && aligned)
                  go(left, chunks.right.align.combine(last.align))
                else
                  go(left, Append(chunks.right, last))
              case _ => Chunks(Append(chunks, last))
            }
        }
        go(chunks, b.unchunk)
      }

    def size = chunks.size
    def sizeLessThan(n: Long) = chunks.sizeLessThan(n)

    def update(n: Long, high: Boolean): BitVector =
      chunks.update(n, high)

    def get(n: Long): Boolean = chunks.get(n)
    def getByte(n: Long): Byte = chunks.getByte(n)
  }

  /** Concatenate `vs` to produce a single `BitVector`. */
  def concat(vs: Vector[BitVector]): BitVector =
    // quite snappy with new algorithm!
    vs.foldLeft(BitVector.empty)(_ ++ _)

  // bit twiddling operations

  private def getBit(byte: Byte, n: Int): Boolean =
    ((0x00000080 >> n) & byte) != 0

  private def setBit(byte: Byte, n: Int, high: Boolean): Byte = {
    if (high) (0x00000080 >> n) | byte
    else (~(0x00000080 >> n)) & byte
  }.toByte

  private def validBitsInLastByte(size: Long): Long = {
    val mod = size % 8
    if (mod == 0) 8 else mod
  }

  /** Gets a byte mask with the top `n` bits enabled. */
  private def topNBits(n: Int): Byte =
    (-1 << (8 - n)).toByte

  private def bytesNeededForBits(size: Long): Long =
    (size + 7) / 8

  /** Returns the bitwise reversal of the provided byte. */
  def reverseBitsInByte(b: Byte): Byte =
    bitReversalTable(b & 0xff)

  private val bitReversalTable: Array[Byte] = Array(
    0x00.toByte,
    0x80.toByte,
    0x40.toByte,
    0xc0.toByte,
    0x20.toByte,
    0xa0.toByte,
    0x60.toByte,
    0xe0.toByte,
    0x10.toByte,
    0x90.toByte,
    0x50.toByte,
    0xd0.toByte,
    0x30.toByte,
    0xb0.toByte,
    0x70.toByte,
    0xf0.toByte,
    0x08.toByte,
    0x88.toByte,
    0x48.toByte,
    0xc8.toByte,
    0x28.toByte,
    0xa8.toByte,
    0x68.toByte,
    0xe8.toByte,
    0x18.toByte,
    0x98.toByte,
    0x58.toByte,
    0xd8.toByte,
    0x38.toByte,
    0xb8.toByte,
    0x78.toByte,
    0xf8.toByte,
    0x04.toByte,
    0x84.toByte,
    0x44.toByte,
    0xc4.toByte,
    0x24.toByte,
    0xa4.toByte,
    0x64.toByte,
    0xe4.toByte,
    0x14.toByte,
    0x94.toByte,
    0x54.toByte,
    0xd4.toByte,
    0x34.toByte,
    0xb4.toByte,
    0x74.toByte,
    0xf4.toByte,
    0x0c.toByte,
    0x8c.toByte,
    0x4c.toByte,
    0xcc.toByte,
    0x2c.toByte,
    0xac.toByte,
    0x6c.toByte,
    0xec.toByte,
    0x1c.toByte,
    0x9c.toByte,
    0x5c.toByte,
    0xdc.toByte,
    0x3c.toByte,
    0xbc.toByte,
    0x7c.toByte,
    0xfc.toByte,
    0x02.toByte,
    0x82.toByte,
    0x42.toByte,
    0xc2.toByte,
    0x22.toByte,
    0xa2.toByte,
    0x62.toByte,
    0xe2.toByte,
    0x12.toByte,
    0x92.toByte,
    0x52.toByte,
    0xd2.toByte,
    0x32.toByte,
    0xb2.toByte,
    0x72.toByte,
    0xf2.toByte,
    0x0a.toByte,
    0x8a.toByte,
    0x4a.toByte,
    0xca.toByte,
    0x2a.toByte,
    0xaa.toByte,
    0x6a.toByte,
    0xea.toByte,
    0x1a.toByte,
    0x9a.toByte,
    0x5a.toByte,
    0xda.toByte,
    0x3a.toByte,
    0xba.toByte,
    0x7a.toByte,
    0xfa.toByte,
    0x06.toByte,
    0x86.toByte,
    0x46.toByte,
    0xc6.toByte,
    0x26.toByte,
    0xa6.toByte,
    0x66.toByte,
    0xe6.toByte,
    0x16.toByte,
    0x96.toByte,
    0x56.toByte,
    0xd6.toByte,
    0x36.toByte,
    0xb6.toByte,
    0x76.toByte,
    0xf6.toByte,
    0x0e.toByte,
    0x8e.toByte,
    0x4e.toByte,
    0xce.toByte,
    0x2e.toByte,
    0xae.toByte,
    0x6e.toByte,
    0xee.toByte,
    0x1e.toByte,
    0x9e.toByte,
    0x5e.toByte,
    0xde.toByte,
    0x3e.toByte,
    0xbe.toByte,
    0x7e.toByte,
    0xfe.toByte,
    0x01.toByte,
    0x81.toByte,
    0x41.toByte,
    0xc1.toByte,
    0x21.toByte,
    0xa1.toByte,
    0x61.toByte,
    0xe1.toByte,
    0x11.toByte,
    0x91.toByte,
    0x51.toByte,
    0xd1.toByte,
    0x31.toByte,
    0xb1.toByte,
    0x71.toByte,
    0xf1.toByte,
    0x09.toByte,
    0x89.toByte,
    0x49.toByte,
    0xc9.toByte,
    0x29.toByte,
    0xa9.toByte,
    0x69.toByte,
    0xe9.toByte,
    0x19.toByte,
    0x99.toByte,
    0x59.toByte,
    0xd9.toByte,
    0x39.toByte,
    0xb9.toByte,
    0x79.toByte,
    0xf9.toByte,
    0x05.toByte,
    0x85.toByte,
    0x45.toByte,
    0xc5.toByte,
    0x25.toByte,
    0xa5.toByte,
    0x65.toByte,
    0xe5.toByte,
    0x15.toByte,
    0x95.toByte,
    0x55.toByte,
    0xd5.toByte,
    0x35.toByte,
    0xb5.toByte,
    0x75.toByte,
    0xf5.toByte,
    0x0d.toByte,
    0x8d.toByte,
    0x4d.toByte,
    0xcd.toByte,
    0x2d.toByte,
    0xad.toByte,
    0x6d.toByte,
    0xed.toByte,
    0x1d.toByte,
    0x9d.toByte,
    0x5d.toByte,
    0xdd.toByte,
    0x3d.toByte,
    0xbd.toByte,
    0x7d.toByte,
    0xfd.toByte,
    0x03.toByte,
    0x83.toByte,
    0x43.toByte,
    0xc3.toByte,
    0x23.toByte,
    0xa3.toByte,
    0x63.toByte,
    0xe3.toByte,
    0x13.toByte,
    0x93.toByte,
    0x53.toByte,
    0xd3.toByte,
    0x33.toByte,
    0xb3.toByte,
    0x73.toByte,
    0xf3.toByte,
    0x0b.toByte,
    0x8b.toByte,
    0x4b.toByte,
    0xcb.toByte,
    0x2b.toByte,
    0xab.toByte,
    0x6b.toByte,
    0xeb.toByte,
    0x1b.toByte,
    0x9b.toByte,
    0x5b.toByte,
    0xdb.toByte,
    0x3b.toByte,
    0xbb.toByte,
    0x7b.toByte,
    0xfb.toByte,
    0x07.toByte,
    0x87.toByte,
    0x47.toByte,
    0xc7.toByte,
    0x27.toByte,
    0xa7.toByte,
    0x67.toByte,
    0xe7.toByte,
    0x17.toByte,
    0x97.toByte,
    0x57.toByte,
    0xd7.toByte,
    0x37.toByte,
    0xb7.toByte,
    0x77.toByte,
    0xf7.toByte,
    0x0f.toByte,
    0x8f.toByte,
    0x4f.toByte,
    0xcf.toByte,
    0x2f.toByte,
    0xaf.toByte,
    0x6f.toByte,
    0xef.toByte,
    0x1f.toByte,
    0x9f.toByte,
    0x5f.toByte,
    0xdf.toByte,
    0x3f.toByte,
    0xbf.toByte,
    0x7f.toByte,
    0xff.toByte
  )

  /** Clears (sets to 0) any bits in the last byte that are not used for storing `size` bits. */
  private def clearUnneededBits(size: Long, bytes: ByteVector): ByteVector = {
    val valid = validBitsInLastByte(size).toInt
    if (bytes.nonEmpty && valid < 8) {
      val idx = bytes.size - 1
      val last = bytes(idx)
      bytes.update(idx, (last & topNBits(valid)).toByte)
    } else
      bytes
  }

  /** Do a 'balanced' reduction of `v`. Provided `f` is associative, this returns the same result as
    * `v.reduceLeft(f)`, but uses a balanced tree of concatenations, which is more efficient for
    * operations that must copy both `A` values to combine them in `f`.
    *
    * Implementation uses a stack that combines the top two elements of the stack using `f` if the
    * top element is more than half the size of the element below it.
    */
  private[bits] def reduceBalanced[A](v: Iterable[A])(size: A => Long)(f: (A, A) => A): A = {
    @annotation.tailrec
    def fixup(stack: List[(A, Long)]): List[(A, Long)] =
      stack match {
        // h actually appeared first in `v`, followed by `h2`, preserve this order
        case (h2, n) :: (h, m) :: t if n > m / 2 =>
          fixup((f(h, h2), m + n) :: t)
        case _ => stack
      }
    v.foldLeft(List[(A, Long)]())((stack, a) => fixup((a -> size(a)) :: stack))
      .reverse
      .map(_._1)
      .reduceLeft(f)
  }

  @SerialVersionUID(1L)
  private class SerializationProxy(private val bytes: Array[Byte], private val size: Long)
      extends Serializable {
    def readResolve: AnyRef = BitVector.view(bytes, size)
  }

  implicit class GroupedOp(val self: BitVector) extends AnyVal {

    /** Converts this vector in to a sequence of `n`-bit vectors.
      * @group collection
      */
    final def grouped(n: Long): Iterator[BitVector] = self.groupedIterator(n)
  }
}
