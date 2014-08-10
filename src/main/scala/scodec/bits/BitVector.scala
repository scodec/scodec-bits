package scodec.bits

import java.nio.{ ByteBuffer, ByteOrder }
import java.security.MessageDigest
import java.util.concurrent.atomic.AtomicLong
import scala.collection.GenTraversableOnce

/**
 * Persistent vector of bits, stored as bytes.
 *
 * Bits are numbered left to right, starting at 0.
 *
 * @groupname collection Collection Like Methods
 * @groupprio collection 0
 *
 * @groupname bitwise Bitwise Operations
 * @groupprio bitwise 1
 *
 * @groupname conversions Conversions
 * @groupprio conversions 2
 *
 * @define bitwiseOperationsReprDescription bit vector
 */
sealed trait BitVector extends BitwiseOperations[BitVector, Long] with Serializable {
  import BitVector._

  /**
   * Returns number of bits in this vector.
   *
   * @group collection
   */
  def size: Long

  /**
   * Alias for [[size]].
   * @group collection
   */
  final def length = size

  /**
   * Returns true if this vector has no bits.
   *
   * @group collection
   */
  final def isEmpty: Boolean = sizeLessThan(1)

  /**
   * Returns true if this vector has a non-zero number of bits.
   *
   * @group collection
   */
  final def nonEmpty: Boolean = !isEmpty

  final protected val sizeLowerBound: AtomicLong = new AtomicLong(0L)
  final protected val sizeUpperBound: AtomicLong = new AtomicLong(Long.MaxValue)

  @annotation.tailrec
  final protected def sizeIsAtLeast(n: Long): Unit = {
    val cur = sizeLowerBound.get
    if (cur >= n) ()
    else if (sizeLowerBound.compareAndSet(cur, n)) ()
    else sizeIsAtLeast(n)
  }

  @annotation.tailrec
  final protected def sizeIsAtMost(n: Long): Unit = {
    val cur = sizeUpperBound.get
    if (cur <= n) ()
    else if (sizeUpperBound.compareAndSet(cur, n)) ()
    else sizeIsAtMost(n)
  }

  /**
   * Returns `true` if the size of this `BitVector` is greater than `n`. Unlike `size`, this
   * forces this `BitVector` from left to right, halting as soon as it has a definite answer.
   *
   * @group collection
   */
  final def sizeGreaterThan(n: Long): Boolean = n < 0 || !sizeLessThanOrEqual(n)

  /**
   * Returns `true` if the size of this `BitVector` is greater than or equal to `n`. Unlike `size`, this
   * forces this `BitVector` from left to right, halting as soon as it has a definite answer.
   *
   * @group collection
   */
  final def sizeGreaterThanOrEqual(n: Long): Boolean = n < 0 || !sizeLessThanOrEqual(n-1)

  /**
   * Returns `true` if the size of this `BitVector` is less than `n`. Unlike `size`, this
   * forces this `BitVector` from left to right, halting as soon as it has a definite answer.
   *
   * @group collection
   */
  final def sizeLessThan(n: Long): Boolean = {
    @annotation.tailrec
    def go(b: BitVector, n: Long): Boolean = {
      if (n < b.sizeLowerBound.get) false
      else if (n > b.sizeUpperBound.get) true
      else b match {
        case Append(l, r) =>
          if (n - l.sizeLowerBound.get < r.sizeLowerBound.get) false
          else if (n - l.sizeUpperBound.get > r.sizeUpperBound.get) true
          else if (l.size >= n) false
          else go(r, n - l.size)
        case s@Suspend(_) => go(s.underlying, n)
        case d: Drop => d.size < n
        case b: Bytes => b.size < n
      }
    }
    val result = go(this, n)
    if (result) sizeIsAtMost(n - 1) else sizeIsAtLeast(n)
    result
  }

  /**
   * Returns `true` if the size of this `BitVector` is less than or equal to `n`. Unlike `size`, this
   * forces this `BitVector` from left to right, halting as soon as it has a definite answer.
   *
   * @group collection
   */
  final def sizeLessThanOrEqual(n: Long): Boolean =
    n == Long.MaxValue || sizeLessThan(n+1)

  /**
   * Returns the number of bits in this vector, or `None` if the size does not
   * fit into an `Int`.
   *
   * @group collection
   */
  final def intSize: Option[Int] = if (size <= Int.MaxValue) Some(size.toInt) else None

  /**
   * Returns true if the `n`th bit is high, false otherwise.
   *
   * @throws NoSuchElementException if `n >= size`
   *
   * @group collection
   */
  def get(n: Long): Boolean

  /**
   * Alias for `get`.
   *
   * @group collection
   * @see get(Long)
   */
  final def apply(n: Long): Boolean = get(n)

  /**
   * Returns `Some(true)` if the `n`th bit is high, `Some(false)` if low, and `None` if `n >= size`.
   *
   * @group collection
   */
  final def lift(n: Long): Option[Boolean] =
    if (sizeGreaterThan(n)) Some(get(n))
    else None

  /**
   * Returns a new bit vector with the `n`th bit high if `high` is true or low if `high` is false.
   *
   * @group collection
   */
  def update(n: Long, high: Boolean): BitVector

  /**
   * Returns a vector with the specified bit inserted at the specified index.
   * @group collection
   */
  final def insert(idx: Long, b: Boolean): BitVector =
    (take(idx) :+ b) ++ drop(idx)

  /**
   * Returns a vector with the specified bit vector inserted at the specified index.
   * @group collection
   */
  final def splice(idx: Long, b: BitVector): BitVector =
    take(idx) ++ b ++ drop(idx)

  /**
   * Returns a vector with the specified bit vector replacing bits `[idx, idx + b.size]`.
   * @group collection
   */
  final def patch(idx: Long, b: BitVector): BitVector =
    take(idx) ++ b ++ drop(idx + b.size)

  /**
   * Returns a new bit vector with the `n`th bit high (and all other bits unmodified).
   *
   * @group collection
   */
  final def set(n: Long): BitVector = update(n, true)

  /**
   * Returns a new bit vector with the `n`th bit low (and all other bits unmodified).
   *
   * @group collection
   */
  final def clear(n: Long): BitVector = update(n, false)

  /**
   * Returns a new bit vector representing this vector's contents followed by the specified vector's contents.
   *
   * @group collection
   */
  def ++(b2: BitVector): BitVector = {
    def go(x: BitVector, y: BitVector, force: Boolean = false): BitVector =
      if ((((x.size+y.size) % 8 == 0) && x.size <= 256 && y.size <= 256) ||
          ((x.size >= 256 && x.size <= 512 && y.size >= 256 && y.size <= 512)))
        // coalesce small bit vectors, preferring to obtain a byte-aligned result
        x.compact.combine(y.compact)
      else if (x.size >= y.size) x match {
        case Append(l,r) if (x.size - y.size) >
                            (r.size - y.size).abs =>
          val r2 = r ++ y
          // if the branches are not of roughly equal size,
          // reinsert the left branch from the top
          if (force || l.size*2 > r2.size) Append(l, r2)
          else go(l, r2, force = true)
        case _ => Append(x, y)
      }
      else y match {
        case Append(l,r) if (y.size - x.size) >
                            (r.size - x.size).abs =>
          val l2 = x ++ l
          if (force || r.size*2 > l2.size) Append(l2, r)
          else go(l2, r, force = true)
        case _ => Append(x, y)
      }
    if (b2.isEmpty) this
    else if (this.isEmpty) b2
    else go(this, b2)
  }

  /**
   * Returns a new vector with the specified bit prepended.
   * @group collection
   */
  final def +:(b: Boolean): BitVector  = BitVector.bit(b) ++ this

  /**
   * Returns a new vector with the specified bit appended.
   * @group collection
   */
  final def :+(b: Boolean): BitVector = this ++ BitVector.bit(b)

  /**
   * Returns `true` if the depth of this tree is `> d`. The result
   * of `compact` has depth 0.
   */
  private[scodec] def depthExceeds(d: Int): Boolean = {
    def go(node: BitVector, cur: Int): Boolean =
      (cur > d) ||
      (node match {
        case Append(l,r) => go(l, cur+1) || go(r, cur+1)
        case Drop(u,n) => go(u, cur+1)
        case Bytes(b,n) => false
        case s@Suspend(_) => go(s.underlying, cur+1)
      })
    go(this, 0)
  }

  /**
   * Returns a vector of all bits in this vector except the first `n` bits.
   *
   * The resulting vector's size is `0 max (size - n)`.
   *
   * @group collection
   */
  final def drop(n: Long): BitVector = {
    @annotation.tailrec
    def go(cur: BitVector, n: Long): BitVector = {
      val npos = n max 0
      if (npos <= 0) cur
      else if (cur.sizeLessThanOrEqual(npos)) BitVector.empty
      else cur match {
        case Bytes(bs, m) =>
          if (npos % 8 == 0) toBytes(bs.drop((npos / 8).toInt), m - npos)
          else Drop(cur.asInstanceOf[Bytes], npos)
        case Append(l, r) =>
          if (l.sizeLessThanOrEqual(npos)) go(r, npos - l.size)
          else Append(l.drop(npos), r) // not stack safe for left-recursion, but we disallow that
        case Drop(bytes, m) => go(bytes, m + npos)
        case s@Suspend(_) => go(s.underlying, npos)
      }
    }
    go(this, n)
  }

  /**
   * Returns a vector of all bits in this vector except the last `n` bits.
   *
   * The resulting vector's size is `0 max (size - n)`.
   *
   * @group collection
   */
  final def dropRight(n: Long): BitVector =
    if (n <= 0) this
    else if (n >= size) BitVector.empty
    else take(size - n)

  /**
   * Returns a vector of the first `n` bits of this vector.
   *
   * The resulting vector's size is `n min size`.
   *
   * Note: if an `n`-bit vector is required, use the `acquire` method instead.
   *
   * @see acquire
   * @group collection
   */
  final def take(n: Long): BitVector = {

    // we have a final value, create a balanced tree from the vectors
    // accumulated to our left, then append that to the final segment
    def finish(accL: Vector[BitVector], lastSegment: BitVector): BitVector =
      if (accL.isEmpty) lastSegment
      else if (lastSegment.isEmpty) reduceBalanced(accL)(_.size)(Append(_,_))
      else Append(reduceBalanced(accL)(_.size)(Append(_,_)), lastSegment)

    // In order to make this function tail recursive, we accumulate a list
    // of bit vectors that we need to concatenate on the left of our final value.
    @annotation.tailrec
    def go(accL: Vector[BitVector], cur: BitVector, n: Long): BitVector = {
      val npos = n max 0
      if (npos == 0) finish(accL, BitVector.empty)
      else if (cur.sizeLessThanOrEqual(npos)) finish(accL, cur)
      else cur match {
        case s@Suspend(_) => go(accL, s.underlying, npos)
        case Bytes(underlying, m) =>
          // eagerly trim from underlying here
          val m2 = npos min m
          val underlyingN = bytesNeededForBits(m2).toInt
          finish(accL, toBytes(underlying.take(underlyingN), m2)) // call to take stack safe here, since that is a Bytes
        case Drop(underlying, m) => finish(accL, underlying.take(m + npos).drop(m))
        case Append(l, r) =>
          if (l.sizeGreaterThanOrEqual(npos)) go(accL, l, npos)
          else go(accL :+ l, r, npos - l.size)
      }
    }
    go(Vector(), this, n)
  }

  /**
   * Returns a vector of the last `n` bits of this vector.
   *
   * The resulting vector's size is `n min size`.
   *
   * @group collection
   */
  final def takeRight(n: Long): BitVector =
    if (n < 0) throw new IllegalArgumentException(s"takeRight($n)")
    else if (n >= size) this
    else this.drop(size-n)


  /**
   * Returns a pair of vectors that is equal to `(take(n), drop(n))`.
   * @group collection
   */
  final def splitAt(n: Long): (BitVector, BitVector) = (take(n), drop(n))

  /**
   * Returns a vector made up of the bits starting at index `from` up to index `until`.
   * @group collection
   */
  final def slice(from: Long, until: Long): BitVector =
    drop(from).take(until - from)

  /**
   * Returns a vector whose contents are the results of taking the first `n` bits of this vector.
   *
   * If this vector does not contain at least `n` bits, an error message is returned.
   *
   * @see take
   * @group collection
   */
  def acquire(n: Long): Either[String, BitVector] =
    if (sizeGreaterThanOrEqual(n)) Right(take(n))
    else Left(s"cannot acquire $n bits from a vector that contains $size bits")

  /**
   * Consumes the first `n` bits of this vector and decodes them with the specified function,
   * resulting in a vector of the remaining bits and the decoded value. If this vector
   * does not have `n` bits or an error occurs while decoding, an error is returned instead.
   *
   * @group collection
   */
  final def consume[A](n: Long)(decode: BitVector => Either[String, A]): Either[String, (BitVector, A)] =
    for {
      toDecode <- acquire(n).right
      decoded <- decode(toDecode).right
    } yield (drop(n), decoded)

  /**
   * Returns true if this bit vector starts with the specified vector.
   * @group collection
   */
  final def startsWith(b: BitVector): Boolean =
    take(b.size) == b

  /**
   * Returns true if this bit vector ends with the specified vector.
   * @group collection
   */
  final def endsWith(b: BitVector): Boolean =
    takeRight(b.size) == b

  /**
   * Finds the first index of the specified bit pattern in this vector.
   * @return index of slice or -1 if not found
   * @group collection
   */
  final def indexOfSlice(slice: BitVector): Long = indexOfSlice(slice, 0)

  /**
   * Finds the first index after `from` of the specified bit pattern in this vector.
   * @return index of slice or -1 if not found
   * @group collection
   */
  final def indexOfSlice(slice: BitVector, from: Long): Long = {
    @annotation.tailrec
    def go(b: BitVector, idx: Long): Long = {
      if (b startsWith slice) idx
      else if (b.isEmpty) -1
      else go(b.tail, idx + 1)
    }
    go(drop(from), from)
  }

  /**
   * Determines if the specified slice is in this vector.
   * @group collection
   */
  final def containsSlice(slice: BitVector): Boolean = indexOfSlice(slice) >= 0

  /**
   * Converts this vector in to a sequence of `n`-bit vectors.
   * @group collection
   */
  final def grouped(n: Long): Stream[BitVector] =
    if (isEmpty) Stream.empty
    else take(n) #:: drop(n).grouped(n)

  /**
   * Returns the first bit of this vector or throws if vector is emtpy.
   * @group collection
   */
  final def head: Boolean = get(0)

  /**
   * Returns the first bit of this vector or `None` if vector is emtpy.
   * @group collection
   */
  final def headOption: Option[Boolean] = lift(0)

  /**
   * Returns a vector of all bits in this vector except the first bit.
   * @group collection
   */
  final def tail: BitVector = drop(1)

  /**
   * Returns a vector of all bits in this vector except the last bit.
   * @group collection
   */
  final def init: BitVector = dropRight(1)

  /**
   * Returns the last bit in this vector or throws if vector is empty.
   * @group collection
   */
  final def last: Boolean = apply(size - 1)

  /**
   * Returns the last bit in this vector or returns `None` if vector is empty.
   * @group collection
   */
  final def lastOption: Option[Boolean] = lift(size - 1)

  /**
   * Alias for `padRight`.
   *
   * @throws IllegalArgumentException if `n < size`
   * @group collection
   */
  final def padTo(n: Long): BitVector = padRight(n)

  /**
   * Returns an `n`-bit vector whose contents are 0 or more low bits followed by this vector's contents.
   *
   * @throws IllegalArgumentException if `n < size`
   * @group collection
   */
  final def padRight(n: Long): BitVector =
    if (n < size) throw new IllegalArgumentException(s"BitVector.padRight($n)")
    else this ++ BitVector.fill(n - size)(false)

  /**
   * Returns an `n`-bit vector whose contents are 0 or more low bits followed by this vector's contents.
   *
   * @throws IllegalArgumentException if `n < size`
   * @group collection
   */
  final def padLeft(n: Long): BitVector =
    if (n < size) throw new IllegalArgumentException(s"BitVector.padLeft($n)")
    else BitVector.fill(n - size)(false) ++ this

  /**
   * Reverse the bits of this vector.
   *
   * @group collection
   */
  final def reverse: BitVector =
    // todo: this has a log time implementation, assuming a balanced tree
    BitVector(compact.underlying.reverse.map(reverseBitsInBytes _)).drop(8 - validBitsInLastByte(size))

  /**
   * Returns a new vector of the same size with the byte order reversed.
   *
   * @group collection
   */
  final def reverseByteOrder: BitVector = {
    if (size % 8 == 0) toBytes(compact.underlying.reverse, size)
    else {
      val validFinalBits = validBitsInLastByte(size)
      val last = take(validFinalBits).compact
      val b = drop(validFinalBits).toByteVector.reverse
      val init = toBytes(b, size-last.size)
      init ++ last
    }
  }

  /**
   * Returns a new vector of the same size with the bit order reversed.
   *
   * @group collection
   */
  final def reverseBitOrder: BitVector =
    BitVector(compact.underlying.map(reverseBitsInBytes _)).drop(8 - validBitsInLastByte(size))

  /**
   * Returns the number of bits that are high.
   *
   * @group bitwise
   */
  final def populationCount: Long = {
    @annotation.tailrec
    def go(b: BitVector, acc: Long): Long = {
      if (b.isEmpty) acc
      else go(b.tail, if (b.head) acc + 1 else acc)
    }
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

  final def shiftRight(n: Long, signExtension: Boolean): BitVector = {
    if (isEmpty || n <= 0) this
    else {
      val extensionHigh = signExtension && head
      if (n >= size) {
        if (extensionHigh) BitVector.high(size) else BitVector.low(size)
      } else {
        (if (extensionHigh) BitVector.high(n) else BitVector.low(n)) ++ dropRight(n)
      }
    }
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

  /**
   * Return a `BitVector` with the same contents as `this`, but
   * based off a single `ByteVector`.
   *
   * This may involve copying data to a fresh `ByteVector`, but
   * has the advantage that lookups index directly into a single
   * `ByteVector` rather than traversing a logarithmic number of nodes
   * in this tree.
   *
   * Calling this method on an already compacted vector is a no-op.
   *
   * @group collection
   */
  final def compact: Bytes = {
    if (bytesNeededForBits(size) > Int.MaxValue)
      throw new IllegalArgumentException(s"cannot compact bit vector of size ${size.toDouble / 8 / 1e9} GB")

    // we collect up all the chunks, then merge them in O(n * log n)
    @annotation.tailrec
    def go(b: BitVector, acc: Vector[Bytes]): Vector[Bytes] = b match {
      case s@Suspend(_) => go(s.underlying, acc)
      case b@Bytes(_,_) => acc :+ b
      case Append(l,r) => go(r, acc :+ l.compact) // not stack safe for left-recursion
      case Drop(b, from) => acc :+ interpretDrop(b,from)
    }

    def interpretDrop(b: Bytes, from: Long): Bytes = {
      val low = from max 0
      val newSize = b.size - low
      if (newSize == 0) BitVector.empty.compact
      else {
        val lowByte = (low / 8).toInt
        val shiftedByWholeBytes = b.underlying.slice(lowByte, lowByte + bytesNeededForBits(newSize).toInt + 1)
        val bitsToShiftEachByte = (low % 8).toInt
        val newBytes = {
          if (bitsToShiftEachByte == 0) shiftedByWholeBytes
          else {
            (shiftedByWholeBytes zipWithI (shiftedByWholeBytes.drop(1) :+ (0: Byte))) { case (a, b) =>
              val hi = (a << bitsToShiftEachByte)
              val low = (((b & topNBits(bitsToShiftEachByte)) & 0x000000ff) >>> (8 - bitsToShiftEachByte))
              hi | low
            }
          }
        }
        toBytes(if (newSize <= (newBytes.size - 1) * 8) newBytes.dropRight(1) else newBytes, newSize)
      }
    }

    this match {
      // common case, we have a single flat `Bytes`, in which case we compact and return it directly
      case bs@Bytes(b,n) =>
        val b2 = b.compact
        if (b2 eq b) bs
        else Bytes(b2,n)
      // other common case is a drop of a single flat `Bytes`
      case Drop(b,from) =>
        val bs = interpretDrop(b,from)
        val b2 = bs.underlying.compact
        if (b2 eq bs.underlying) bs
        else Bytes(b2, bs.size)
      // otherwise we fall back to general purpose algorithm
      case _ => reduceBalanced(go(this, Vector()))(_.size)(_ combine _) match {
        case Bytes(b,n) => Bytes(b.compact,n) // we compact the underlying ByteVector as well
      }
    }
  }

  /**
   * Return a `BitVector` with the same contents as `this`, but
   * based off a single flat `ByteVector`. This function is guaranteed
   * to copy all the bytes in this `BitVector`, unlike `compact`, which
   * may no-op if this `BitVector` already consists of a single `ByteVector`
   * chunk.
   *
   * @group collection
   */
  final def copy: Bytes = this match {
    case Bytes(b,n) => Bytes(b.copy, n)
    case _ => this.compact
  }

  /**
   * Forces any `Suspend` nodes in this `BitVector` and ensures the tree is balanced.
   *
   * @group collection
   */
  final def force: BitVector = {
    @annotation.tailrec
    def go(accL: Vector[BitVector], cur: BitVector): BitVector =
      cur match {
        case b@Bytes(_,_) => (accL :+ b).reduce(_ ++ _)
        case Append(l,r) => go(accL :+ l.force, r)
        case d@Drop(_, _) => (accL :+ d).reduce(_ ++ _)
        case s@Suspend(_) => go(accL, s.underlying)
      }
    go(Vector(), this)
  }

  /**
   * Return the sequence of bits in this vector. The returned
   * `IndexedSeq` is just a view; nothing is actually copied.
   *
   * @throws IllegalArgumentException if this vector's size exceeds Int.MaxValue
   * @see acquire
   * @see toIndexedSeq
   * @group conversions
   */
  final def toIndexedSeq: IndexedSeq[Boolean] = {
    intSize.map { n =>
      new IndexedSeq[Boolean] {
        def length = BitVector.this.size.toInt
        def apply(idx: Int): Boolean = BitVector.this.get(idx.toLong)
      }
    }.getOrElse {
      throw new IllegalArgumentException(s"BitVector too big for Seq: $size")
    }
  }

  /**
   * Converts the contents of this vector to a byte vector.
   *
   * If this vector's size does not divide evenly by 8, the last byte of the returned vector
   * will be zero-padded to the right.
   *
   * @group conversions
   */
  final def toByteVector: ByteVector =
    clearUnneededBits(size, compact.underlying)

  /**
   * Alias for [[toByteVector]].
   * @group conversions
   */
  final def bytes: ByteVector = toByteVector

  /**
   * Converts the contents of this vector to a byte array.
   *
   * If this vector's size does not divide evenly by 8, the last byte of the returned vector
   * will be zero-padded to the right.
   *
   * @group conversions
   */
  final def toByteArray: Array[Byte] = toByteVector.toArray

  /**
   * Converts the contents of this vector to a `java.nio.ByteBuffer`.
   *
   * The returned buffer is freshly allocated with limit set to the minimum number of bytes needed
   * to represent the contents of this vector, position set to zero, and remaining set to the limit.
   *
   * @see toByteVector
   * @group conversions
   */
  final def toByteBuffer: java.nio.ByteBuffer = toByteVector.toByteBuffer

  /**
   * Converts the contents of this bit vector to a binary string of `size` digits.
   *
   * @group conversions
   */
  final def toBin: String = toByteVector.toBin.take(size.toInt)

  /**
   * Converts the contents of this bit vector to a binary string of `size` digits.
   *
   * @group conversions
   */
  final def toBin(alphabet: Bases.BinaryAlphabet): String = toByteVector.toBin(alphabet).take(size.toInt)

  /**
   * Converts the contents of this bit vector to a hexadecimal string of `ceil(size / 4)` nibbles.
   *
   * The last nibble is right-padded with zeros if the size is not evenly divisible by 4.
   *
   * @group conversions
   */
  final def toHex: String = toHex(Bases.Alphabets.HexLowercase)

  /**
   * Converts the contents of this bit vector to a hexadecimal string of `ceil(size / 4)` nibbles.
   *
   * The last nibble is right-padded with zeros if the size is not evenly divisible by 4.
   *
   * @group conversions
   */
  final def toHex(alphabet: Bases.HexAlphabet): String = {
    val full = toByteVector.toHex(alphabet)
    size % 8 match {
      case 0 => full
      case n if n <= 4 => full.init
      case other => full
    }
  }

  /**
   * Converts the contents of this vector to a base 64 string.
   *
   * The last byte is right-padded with zeros if the size is not evenly divisible by 8.
   *
   * @group conversions
   */
  final def toBase64: String = toBase64(Bases.Alphabets.Base64)

  /**
   * Converts the contents of this vector to a base 64 string using the specified alphabet.
   *
   * The last byte is right-padded with zeros if the size is not evenly divisible by 8.
   *
   * @group conversions
   */
  final def toBase64(alphabet: Bases.Base64Alphabet): String = toByteVector.toBase64(alphabet)

  /**
   * Converts the contents of this vector to an int.
   *
   * @param signed whether sign extension should be performed
   * @param ordering order bytes should be processed in
   * @throws IllegalArgumentException if size is greater than 32
   * @group conversions
   */
  final def toInt(signed: Boolean = true, ordering: ByteOrdering = ByteOrdering.BigEndian): Int = {
    require(sizeLessThanOrEqual(32))
    val bits = intSize.get
    val mod = bits % 8
    var result = 0
    ordering match {
      case ByteOrdering.BigEndian =>
        @annotation.tailrec
        def go(bv: ByteVector): Unit =
          if (bv.nonEmpty) {
            result = (result << 8) | (0x0ff & bv.head)
            go(bv.tail)
          }
        go(this.bytes)
      case ByteOrdering.LittleEndian =>
        @annotation.tailrec
        def go(bv: ByteVector, i: Int): Unit =
          if (bv.nonEmpty) {
            result = result | ((0x0ff & bv.head) << (8 * i))
            go(bv.tail, i + 1)
          }
        go(this.bytes, 0)
    }
    if (mod != 0) result = result >>> (8 - mod)
    // Sign extend if necessary
    if (signed && bits != 32 && ((1 << (bits - 1)) & result) != 0) {
      val toShift = 32 - bits
      result = (result << toShift) >> toShift
    }
    result
  }

  /**
   * Converts the contents of this vector to a long.
   *
   * @param signed whether sign extension should be performed
   * @param ordering order bytes should be processed in
   * @throws IllegalArgumentException if size is greater than 64
   * @group conversions
   */
  final def toLong(signed: Boolean = true, ordering: ByteOrdering = ByteOrdering.BigEndian): Long = {
    require(sizeLessThanOrEqual(64))
    val bits = intSize.get
    val mod = bits % 8
    var result = 0L
    ordering match {
      case ByteOrdering.BigEndian =>
        @annotation.tailrec
        def go(bv: ByteVector): Unit =
          if (bv.nonEmpty) {
            result = (result << 8) | (0x0ffL & bv.head)
            go(bv.tail)
          }
        go(this.bytes)
      case ByteOrdering.LittleEndian =>
        @annotation.tailrec
        def go(bv: ByteVector, i: Int): Unit =
          if (bv.nonEmpty) {
            result = result | ((0x0ffL & bv.head) << (8 * i))
            go(bv.tail, i + 1)
          }
        go(this.bytes, 0)
    }
    if (mod != 0) result = result >>> (8 - mod)
    // Sign extend if necessary
    if (signed && bits != 64 && ((1 << (bits - 1)) & result) != 0) {
      val toShift = 64 - bits
      result = (result << toShift) >> toShift
    }
    result
  }

  /**
   * Computes a digest of this bit vector.
   * The last byte is zero padded if the size is not evenly divisible by 8.
   * @param algorithm digest algorithm to use
   * @group conversions
   */
  final def digest(algorithm: String): BitVector = digest(MessageDigest.getInstance(algorithm))

  /**
   * Computes a digest of this bit vector.
   * The last byte is zero padded if the size is not evenly divisible by 8.
   * @param digest digest to use
   * @group conversions
   */
  final def digest(digest: MessageDigest): BitVector = BitVector(bytes.digest(digest))

  /**
   * Returns true if the specified value is a `BitVector` with the same contents as this vector.
   * @group collection
   */
  override final def equals(other: Any): Boolean = other match {
    case o: BitVector => {
      val chunkSize = 8 * 1024 * 64
      @annotation.tailrec
      def go(x: BitVector, y: BitVector): Boolean = {
        if (x.isEmpty) y.isEmpty
        else {
          val chunkX = x.take(chunkSize)
          val chunkY = y.take(chunkSize)
          chunkX.size == chunkY.size &&
          chunkX.toByteVector == chunkY.toByteVector &&
          go(x.drop(chunkSize), y.drop(chunkSize))
        }
      }
      go(this, o)
    }
    case _ => false
  }

  /**
   * Calculates the hash code of this vector. The result is cached.
   * @group collection
   */
  override final lazy val hashCode = {
    // todo: this could be recomputed more efficiently using the tree structure
    // given an associative hash function
    import util.hashing.MurmurHash3._
    val chunkSize = 8 * 1024 * 64
    @annotation.tailrec
    def go(bits: BitVector, h: Int): Int = {
      if (bits.isEmpty) finalizeHash(h, (size % Int.MaxValue.toLong).toInt + 1)
      else go(bits.drop(chunkSize), mix(h, bytesHash(bits.take(chunkSize).toByteArray)))
    }
    go(this, stringHash("BitVector"))
  }

  /**
   * Display the size and bytes of this `BitVector`.
   * For bit vectors beyond a certain size, only a hash of the
   * contents are shown.
   * @group collection
   */
  override final def toString =
    if (isEmpty) "BitVector(empty)"
    else if (sizeLessThan(513)) s"BitVector($size bits, 0x${toHex})"
    else s"BitVector($size bits, #${hashCode})"

  // impl details

  final protected def checkBounds(n: Long): Unit =
    if (!sizeGreaterThan(n)) outOfBounds(n)

  final protected def outOfBounds(n: Long): Nothing =
    throw new NoSuchElementException(s"invalid index: $n of $size")

  final protected def mapBytes(f: ByteVector => ByteVector): BitVector = this match {
    case Bytes(bs, n) => toBytes(f(bs), n)
    case Append(l,r) => Append(l.mapBytes(f), r.mapBytes(f))
    case Drop(b,n) => Drop(b.mapBytes(f).compact, n)
    case s@Suspend(_) => Suspend(() => s.underlying.mapBytes(f))
  }

  /**
   * Pretty print this `BitVector`.
   */
  private[scodec] def internalPretty(prefix: String): String = this match {
    case Append(l,r) => prefix + "append\n" +
                        l.internalPretty(prefix + "  ") + "\n" +
                        r.internalPretty(prefix + "  ")
    case Bytes(b, n) =>
      if (n > 16) prefix + s"bits $n #:${b.hashCode}"
      else        prefix + s"bits $n 0x${b.toHex}"
    case Drop(u, n) => prefix + s"drop ${n}\n" +
                       u.internalPretty(prefix + "  ")
    case s@Suspend(_) => s.underlying.internalPretty(prefix)
  }

  private def zipBytesWith(other: BitVector)(op: (Byte, Byte) => Int): BitVector = {
    // todo: this has a much more efficient recursive algorithm -
    // only need to compact close to leaves of the tree
    toBytes(this.compact.underlying.zipWithI(other.compact.underlying)(op), this.size min other.size)
  }

  protected final def writeReplace(): AnyRef = new SerializationProxy(toByteArray, size)
}

/**
 * Companion for [[BitVector]].
 *
 * @groupname constants Constants
 * @groupprio constants 0
 *
 * @groupname constructors Constructors
 * @groupprio constructors 1
 *
 * @groupname base Base Conversions
 * @groupprio base 2
 */
object BitVector {

  /**
   * Empty bit vector.
   * @group constants
   */
  val empty: BitVector = toBytes(ByteVector.empty, 0)

  /**
   * 1-bit vector with only bit set low.
   * @group constants
   */
  val zero: BitVector = toBytes(ByteVector(0), 1)

  /**
   * 1-bit vector with only bit set high.
   * @group constants
   */
  val one: BitVector = toBytes(ByteVector(0xff), 1)

  /**
   * 8-bit vector with all bits set low.
   * @group constants
   */
  val lowByte: BitVector = toBytes(ByteVector.fill(8)(0), 8)

  /**
   * 8-bit vector with all bits set high.
   * @group constants
   */
  val highByte: BitVector = toBytes(ByteVector.fill(8)(1), 8)

  /**
   * 1-bit vector with only bit set to specified value.
   * @group constructors
   */
  def bit(high: Boolean): BitVector = if (high) one else zero

  /**
   * n-bit vector with bit at index `i` set to value of boolean at index `i` in specified iterable.
   * @group constructors
   */
  def bits(b: Iterable[Boolean]): BitVector =
    b.zipWithIndex.foldLeft(low(b.size))((acc,b) =>
      acc.update(b._2, b._1)
    )

  /**
   * n-bit vector with all bits set high.
   * @group constructors
   */
  def high(n: Long): BitVector = fill(n)(true)

  /**
   * n-bit vector with all bits set low.
   * @group constructors
   */
  def low(n: Long): BitVector = fill(n)(false)

  /**
   * Constructs a `BitVector` from a `ByteVector`.
   * This method has runtime O(1).
   * @group constructors
   */
  def apply(bs: ByteVector): BitVector = toBytes(bs, bs.size.toLong * 8)

  /**
   * Constructs a `BitVector` from a `ByteBuffer`. The given `ByteBuffer` is
   * is copied to ensure the resulting `BitVector` is immutable.
   * If this is not desired, use `[[BitVector.view]]`.
   * @group constructors
   */
  def apply(buffer: ByteBuffer): BitVector = apply(ByteVector(buffer))

  /**
   * Constructs a `BitVector` from an `Array[Byte]`. The given `Array[Byte]` is
   * is copied to ensure the resulting `BitVector` is immutable.
   * If this is not desired, use `[[BitVector.view]]`.
   * @group constructors
   */
  def apply(bs: Array[Byte]): BitVector = toBytes(ByteVector(bs), bs.size.toLong * 8)

  /**
   * Constructs a `BitVector` from a collection of bytes.
   * @group constructors
   */
  def apply(bs: GenTraversableOnce[Byte]): BitVector = apply(ByteVector(bs))

  /**
   * Constructs a `BitVector` from a list of literal bytes. Only the least significant
   * byte is used of each integral value.
   * @group constructors
   */
  def apply[A: Integral](bytes: A*): BitVector = apply(ByteVector(bytes: _*))

  /**
   * Constructs a `BitVector` from a `ByteBuffer` using the buffer limit * 8 as the size.
   * Unlike `apply`, this does not make a copy of the input buffer, so callers should take care
   * not to modify the contents of the buffer passed to this function.
   * @group constructors
   */
  def view(buffer: ByteBuffer): BitVector = toBytes(ByteVector.view(buffer), buffer.limit.toLong * 8)

  /**
   * Constructs a `BitVector` from the first `sizeInBits` of the `ByteBuffer`.
   * Unlike `apply`, this does not make a copy of the input buffer, so callers should take care
   * not to modify the contents of the buffer passed to this function.
   * @group constructors
   */
  def view(buffer: ByteBuffer, sizeInBits: Long): BitVector = {
    require(bytesNeededForBits(sizeInBits) <= Int.MaxValue,
      "Cannot have BitVector chunk larger than Int.MaxValue bytes: " + sizeInBits)
    toBytes(ByteVector.view(ind => buffer.get(ind.toInt), bytesNeededForBits(sizeInBits).toInt), sizeInBits)
  }

  /**
   * Constructs a `BitVector` from an `Array[Byte]`. Unlike `apply`, this
   * does not make a copy of the input array, so callers should take care
   * not to modify the contents of the array passed to this function.
   * @group constructors
   */
  def view(bs: Array[Byte]): BitVector = view(bs, bs.size.toLong * 8)

  /**
   * Constructs a `BitVector` from an `Array[Byte]`. Unlike `apply`, this
   * does not make a copy of the input array, so callers should take care
   * not to modify the contents of the array passed to this function.
   * @group constructors
   */
  def view(bs: Array[Byte], sizeInBits: Long): BitVector = toBytes(ByteVector.view(bs), sizeInBits)

  /**
   * Constructs an `n`-bit `BitVector` where each bit is set to the specified value.
   * @group constructors
   */
  def fill(n: Long)(high: Boolean): BitVector = {
    val needed = bytesNeededForBits(n)
    if (needed < Int.MaxValue) {
      val bs = ByteVector.fill(needed.toInt)(if (high) -1 else 0)
      toBytes(bs, n)
    }
    else {
      fill(n / 2)(high) ++ fill(n - (n/2))(high)
    }
  }

  /**
   * Constructs a bit vector with the 2's complement encoding of the specified value.
   * @param i value to encode
   * @param size size of vector (<= 32)
   * @param ordering byte ordering of vector
   */
  def fromInt(i: Int, size: Int = 32, ordering: ByteOrdering = ByteOrdering.BigEndian): BitVector = {
    require(size <= 32)
    val buffer = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(i)
    buffer.flip()
    val relevantBits = (BitVector.view(buffer) << (32 - size)).take(size)
    if (ordering == ByteOrdering.BigEndian) relevantBits else relevantBits.reverseByteOrder
  }

  /**
   * Constructs a bit vector with the 2's complement encoding of the specified value.
   * @param l value to encode
   * @param size size of vector (<= 64)
   * @param ordering byte ordering of vector
   */
  def fromLong(l: Long, size: Int = 64, ordering: ByteOrdering = ByteOrdering.BigEndian): BitVector = {
    require(size <= 64)
    val buffer = ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putLong(l)
    buffer.flip()
    val relevantBits = (BitVector.view(buffer) << (64 - size)).take(size)
    if (ordering == ByteOrdering.BigEndian) relevantBits else relevantBits.reverseByteOrder
  }

  /**
   * Constructs a `BitVector` from a binary string or returns an error message if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   * @group base
   */
  def fromBinDescriptive(str: String, alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary): Either[String, BitVector] =
    ByteVector.fromBinInternal(str, alphabet).right.map { case (bytes, size) =>
      val toDrop = size match {
        case 0 => 0
        case n if n % 8 == 0 => 0
        case n => 8 - (n % 8)
      }
      bytes.toBitVector.drop(toDrop)
    }

  /**
   * Constructs a `ByteVector` from a binary string or returns `None` if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   * @group base
   */
  def fromBin(str: String, alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary): Option[BitVector] = fromBinDescriptive(str, alphabet).right.toOption

  /**
   * Constructs a `ByteVector` from a binary string or throws an IllegalArgumentException if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   *
   * @throws IllegalArgumentException if the string is not valid hexadecimal
   * @group base
   */
  def fromValidBin(str: String, alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary): BitVector =
    fromBinDescriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)

  /**
   * Constructs a `BitVector` from a hexadecimal string or returns an error message if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   * @group base
   */
  def fromHexDescriptive(str: String, alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase): Either[String, BitVector] =
    ByteVector.fromHexInternal(str, alphabet).right.map { case (bytes, count) =>
      val toDrop = if (count % 2 == 0) 0 else 4
      bytes.toBitVector.drop(toDrop)
    }

  /**
   * Constructs a `BitVector` from a hexadecimal string or returns `None` if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   * @group base
   */
  def fromHex(str: String, alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase): Option[BitVector] = fromHexDescriptive(str).right.toOption

  /**
   * Constructs a `BitVector` from a hexadecimal string or throws an IllegalArgumentException if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   *
   * @throws IllegalArgumentException if the string is not valid hexadecimal
   * @group base
   */
  def fromValidHex(str: String, alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase): BitVector =
    fromHexDescriptive(str).fold(msg => throw new IllegalArgumentException(msg), identity)

  /**
   * Constructs a `BitVector` from a base 64 string or returns an error message if the string is not valid base 64.
   *
   * The string may contain whitespace characters.
   * @group base
   */
  def fromBase64Descriptive(str: String, alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64): Either[String, BitVector] =
    ByteVector.fromBase64Descriptive(str, alphabet).right.map { _.toBitVector }

  /**
   * Constructs a `BitVector` from a base 64 string or returns `None` if the string is not valid base 64.
   *
   * The string may contain whitespace characters.
   * @group base
   */
  def fromBase64(str: String, alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64): Option[BitVector] = fromBase64Descriptive(str, alphabet).right.toOption

  /**
   * Constructs a `BitVector` from a base 64 string or throws an IllegalArgumentException if the string is not valid base 64.
   *
   * The string may contain whitespace characters.
   *
   * @throws IllegalArgumentException if the string is not valid base 64
   * @group base
   */
  def fromValidBase64(str: String, alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64): BitVector =
    fromBase64Descriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)

  /**
   * Create a lazy `BitVector` by repeatedly extracting chunks from `S`.
   * The returned `BitVector` will have the structure of a fully lazy
   * right-associated cons list. Thus, `get`, `take`, and `drop` will
   * be efficient when operating on the head of the list, but accessing
   * later indices (for `takeRight`, say, or `get(size-1)` will require
   * forcing the stream up to that point.
   *
   * Use `force` if you wish to convert the result to an in-memory strict
   * `BitVector` backed by a balanced tree.
   *
   * @group constructors
   */
  def unfold[S](s: S)(f: S => Option[(BitVector, S)]): BitVector =
    Suspend { () => f(s).map { case (h,t) => Append(h, unfold(t)(f)) }
                        .getOrElse { BitVector.empty } }

  /**
   * Produce a lazy `BitVector` from the given `InputStream`, using `chunkSizeInBytes`
   * to control the number of bytes read in each chunk (defaulting to 4MB).
   * This simply calls [[scodec.bits.BitVector.unfold]] with a function to extract a series
   * of flat byte arrays from the `InputStream`.
   *
   * This function does not handle closing the `InputStream` and has all the usual
   * drawbacks of lazy I/O - `I/O` exceptions may be raised unexpectedly in pure code as
   * chunks are forced, and it must memoize the results to prevent the underlying side
   * effects from being observed. Streaming applications should take care to ensure
   * that the head of the stream is not left on the stack, as this will cause the entire
   * stream to be retained in memory.
   *
   * @param chunkSizeInBytes the number of bytes to read in each chunk
   * @group constructors
   */
  def fromInputStream(in: java.io.InputStream, chunkSizeInBytes: Int = 1024 * 1000 * 16): BitVector =
    unfold(in) { in =>
      val buf = new Array[Byte](chunkSizeInBytes)
      val nRead = in.read(buf)
      if (nRead == chunkSizeInBytes) Some((BitVector(buf), in))
      else if (nRead == -1) None
      else Some((BitVector(buf.take(nRead): Array[Byte]), in))
    }

  /**
   * Produce a lazy `BitVector` from the given `ReadableByteChannel`, using `chunkSizeInBytes`
   * to control the number of bytes read in each chunk (defaulting to 8k). This function
   * does lazy I/O, see [[scodec.bits.BitVector.fromInputStream]] for caveats. The `direct`
   * parameter, if `true`, allows for (but does not enforce) using a 'direct' [[java.nio.ByteBuffer]]
   * for each chunk, which means the buffer and corresponding `BitVector` chunk may be backed by a
   * 'view' rather than an in-memory array. This may be more efficient for some workloads. See
   * [[java.nio.ByteBuffer]] for more information.
   *
   * @param chunkSizeInBytes the number of bytes to read in each chunk
   * @param direct true if we should attempt to use a 'direct' [[java.nio.ByteBuffer]] for reads
   * @group constructors
   */
  def fromChannel(in: java.nio.channels.ReadableByteChannel, chunkSizeInBytes: Int = 1024 * 1000 * 16,
                  direct: Boolean = false): BitVector =
    unfold(in) { in =>
      val buf = if (direct) java.nio.ByteBuffer.allocateDirect(chunkSizeInBytes)
                else        java.nio.ByteBuffer.allocate(chunkSizeInBytes)
      val nRead = in.read(buf)
      buf.flip
      if (nRead != -1) Some((BitVector.view(buf, nRead.toLong*8), in))
      else None
    }

  /**
   * Produce a lazy `BitVector` from the given `FileChannel`, using `chunkSizeInBytes`
   * to control the number of bytes read in each chunk (defaulting to 16MB). Unlike
   * [[scodec.bits.BitVector.fromChannel]], this memory-maps chunks in, rather than copying
   * them explicitly.
   *
   * Behavior is unspecified if this function is used concurrently with the underlying
   * file being written.
   *
   * @param chunkSizeInBytes the number of bytes to read in each chunk
   * @group constructors
   */
  def fromMmap(in: java.nio.channels.FileChannel, chunkSizeInBytes: Int = 1024 * 1000 * 16): BitVector =
    unfold(in -> 0L) { case (in,pos) =>
      if (pos == in.size) None
      else {
        require (pos < in.size)
        val bytesToRead = (in.size - pos) min chunkSizeInBytes.toLong
        val buf = in.map(java.nio.channels.FileChannel.MapMode.READ_ONLY, pos, bytesToRead)
        require(buf.limit == bytesToRead)
        Some((BitVector.view(buf), (in -> (pos + bytesToRead))))
      }
    }

  /** Smart constructor for `Bytes`. */
  private[scodec] def toBytes(bs: ByteVector, sizeInBits: Long): Bytes = {
    val needed = bytesNeededForBits(sizeInBits)
    require(needed <= bs.size)
    val b = if (bs.size > needed) bs.take(needed.toInt) else bs
    Bytes(b, sizeInBits)
  }

  private[scodec] case class Bytes(val underlying: ByteVector, val size: Long) extends BitVector {
    private val invalidBits = 8 - validBitsInLastByte(size)
    def get(n: Long): Boolean = {
      checkBounds(n)
      getBit(underlying((n / 8).toInt), (n % 8).toInt)
    }
    def update(n: Long, high: Boolean): BitVector = {
      checkBounds(n)
      val b2 = underlying.update(
        (n / 8).toInt,
        underlying.lift((n / 8).toInt).map(setBit(_, (n % 8).toInt, high)).getOrElse {
          outOfBounds(n)
        }
      )
      Bytes(b2, size)
    }
    def combine(other: Bytes): Bytes = {
      val otherBytes = other.underlying
      if (isEmpty) {
        other
      } else if (otherBytes.isEmpty) {
        this
      } else if (invalidBits == 0) {
        toBytes(underlying ++ otherBytes, size + other.size)
      } else {
        val bytesCleared = clearUnneededBits(size, underlying) // this is key
        val hi = bytesCleared(bytesCleared.size - 1)
        val lo = (((otherBytes.head & topNBits(invalidBits.toInt)) & 0x000000ff) >>> validBitsInLastByte(size)).toByte
        val updatedOurBytes = bytesCleared.update(bytesCleared.size - 1, (hi | lo).toByte)
        val updatedOtherBytes = other.drop(invalidBits).toByteVector
        toBytes(updatedOurBytes ++ updatedOtherBytes, size + other.size)
      }
    }
  }

  private[scodec] case class Drop(underlying: Bytes, m: Long) extends BitVector {
    val size = (underlying.size - m) max 0
    def get(n: Long): Boolean =
      underlying.get(m + n)
    def update(n: Long, high: Boolean): BitVector =
      Drop(underlying.update(m + n, high).compact, m)
  }
  private[scodec] case class Append(left: BitVector, right: BitVector) extends BitVector {
    lazy val size = {
      val sz = left.size + right.size
      sizeLowerBound.set(sz)
      sizeUpperBound.set(sz)
      sz
    }
    def get(n: Long): Boolean =
      if (n < left.size) left.get(n)
      else right.get(n - left.size)
    def update(n: Long, high: Boolean): BitVector =
      if (n < left.size) Append(left.update(n, high), right)
      else Append(left, right.update(n - left.size, high))
  }
  private[scodec] case class Suspend(thunk: () => BitVector) extends BitVector {
    lazy val underlying = thunk()
    def get(n: Long): Boolean = underlying.get(n)
    def update(n: Long, high: Boolean): BitVector = underlying.update(n, high)
    def size = underlying.size
  }

  // bit twiddling operations

  private def getBit(byte: Byte, n: Int): Boolean =
    ((0x00000080 >> n) & byte) != 0

  private def setBit(byte: Byte, n: Int, high: Boolean): Byte = {
    if (high) (0x00000080 >> n) | byte
    else (~(0x00000080 >> n)) & byte
  }.toByte

  private def validBitsInLastByte(size: Long): Long = {
    val mod = size % 8
    (if (mod == 0) 8 else mod)
  }

  /** Gets a byte mask with the top `n` bits enabled. */
  private def topNBits(n: Int): Byte =
    (-1 << (8 - n)).toByte

  private def bytesNeededForBits(size: Long): Long =
    (size + 7) / 8

  private def reverseBitsInBytes(b: Byte): Byte = {
    // See Hacker's Delight Chapter 7 page 101
    var x = (b & 0x055) << 1 | (b & 0x0aa) >> 1
    x = (x & 0x033) << 2 | (x & 0x0cc) >> 2
    x = (x & 0x00f) << 4 | (x & 0x0f0) >> 4
    x.toByte
  }

  /** Clears (sets to 0) any bits in the last byte that are not used for storing `size` bits. */
  private def clearUnneededBits(size: Long, bytes: ByteVector): ByteVector = {
    val valid = validBitsInLastByte(size).toInt
    if (bytes.nonEmpty && valid < 8) {
      val idx = bytes.size - 1
      val last = bytes(idx)
      bytes.update(idx, (last & topNBits(valid)).toByte)
    } else {
      bytes
    }
  }

  /**
   * Do a 'balanced' reduction of `v`. Provided `f` is associative, this
   * returns the same result as `v.reduceLeft(f)`, but uses a balanced
   * tree of concatenations, which is more efficient for operations that
   * must copy both `A` values to combine them in `f`.
   *
   * Implementation uses a stack that combines the top two elements of the
   * stack using `f` if the top element is more than half the size of the
   * element below it.
   */
  private def reduceBalanced[A](v: TraversableOnce[A])(size: A => Long)(
                                f: (A,A) => A): A = {
    @annotation.tailrec
    def fixup(stack: List[(A,Long)]): List[(A,Long)] = stack match {
      // h actually appeared first in `v`, followed by `h2`, preserve this order
      case (h2,n) :: (h,m) :: t if n > m/2 =>
        fixup { (f(h, h2), m+n) :: t }
      case _ => stack
    }
    v.foldLeft(List[(A,Long)]())((stack,a) => fixup((a -> size(a)) :: stack))
     .reverse.map(_._1)
     .reduceLeft(f)
  }

  @SerialVersionUID(1L)
  private class SerializationProxy(private val bytes: Array[Byte], private val size: Long) extends Serializable {
    def readResolve: AnyRef = BitVector.view(bytes, size)
  }
}

