package scodec.bits

import ByteVector._
import collection.immutable.Queue
import java.nio.ByteBuffer
import java.security.MessageDigest
import scala.collection.GenTraversableOnce
import java.io.OutputStream
import java.util.concurrent.atomic.AtomicLong

/**
 * An immutable vector of bytes, backed by a balanced binary tree of
 * chunks. Most operations are logarithmic in the depth of this tree,
 * including `++`, `:+`, `+:`, `update`, and `insert`. Where possible,
 * operations return lazy views rather than copying any underlying bytes.
 * Use `copy` to copy all underlying bytes to a fresh, array-backed `ByteVector`.
 *
 * Unless otherwise noted, operations follow the same naming as the scala
 * standard library collections, though this class does not extend any of the
 * standard scala collections. Use `toIndexedSeq`, `toSeq`, or `toIterable`
 * to obtain a regular `scala.collection` type.
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
 * @define returnsView This method returns a view and hence, is O(1). Call [[compact]] generate a new strict vector.
 */
sealed trait ByteVector extends BitwiseOperations[ByteVector,Int] with Serializable {

  /**
   * Returns the number of bytes in this vector.
   * @group collection
   */
  def size: Int

  /**
   * Alias for [[size]].
   * @group collection
   */
  final def length = size

  /**
   * Returns true if this vector has no bytes.
   * @group collection
   */
  final def isEmpty = size == 0

  /**
   * Returns true if this vector has a non-zero number of bytes.
   * @group collection
   */
  final def nonEmpty: Boolean = !isEmpty

  /**
   * Gets the byte at the specified index.
   * @throws IndexOutOfBoundsException if the specified index is not in `[0, size)`
   * @group collection
   */
  final def get(index: Int): Byte = {
    checkIndex(index)
    @annotation.tailrec
    def go(cur: ByteVector, n: Int): Byte = cur match {
      case Append(l,r) => if (n < l.size) go(l, n)
                          else go(r, n-l.size)
      case Chunk(arr) => arr(n)
      case Buffer(_, _, tl) => tl.get(n)
    }
    go(this, index)
  }

  /**
   * Alias for [[get]].
   * @throws IndexOutOfBoundsException if the specified index is not in `[0, size)`
   * @group collection
   */
  final def apply(index: Int): Byte = get(index)

  /**
   * Returns the byte at the specified index, or `None` if the index is out of range.
   * @group collection
   */
  final def lift(index: Int): Option[Byte] = {
    if (index >= 0 && index < size) Some(apply(index))
    else None
  }

  /**
   * Returns a vector with the byte at the specified index replaced with the specified byte.
   * @group collection
   */
  final def update(idx: Int, b: Byte): ByteVector = {
    checkIndex(idx)
    (take(idx) :+ b) ++ drop(idx+1)
  }

  /**
   * Returns a vector with the specified byte inserted at the specified index.
   * @group collection
   */
  final def insert(idx: Int, b: Byte): ByteVector =
    (take(idx) :+ b) ++ drop(idx)

  /**
   * Returns a vector with the specified byte vector inserted at the specified index.
   * @group collection
   */
  final def splice(idx: Int, b: ByteVector): ByteVector =
    take(idx) ++ b ++ drop(idx)

  /**
   * Returns a vector with the specified byte vector replacing bytes `[idx, idx + b.size]`.
   * @group collection
   */
  final def patch(idx: Int, b: ByteVector): ByteVector =
    take(idx) ++ b ++ drop(idx + b.size)

  /**
   * Returns a new byte vector representing this vector's contents followed by the specified vector's contents.
   * @group collection
   */
  final def ++(other: ByteVector): ByteVector = {
    def go(x: ByteVector, y: ByteVector, force: Boolean): ByteVector =
      if (x.size >= y.size) x match {
        case Append(l,r) if (x.size - y.size) > // only descend into subtree if its a closer
                            (r.size - y.size).abs => // match in size
          val r2 = r ++ y
          // if the branches are not of roughly equal size,
          // reinsert the left branch from the top
          if (force || l.size*2 > r2.size) Append(l, r2)
          else go(l, r2, force = true)
        case _ => Append(x, y) // otherwise, insert at the 'top'
      }
      else y match {
        case Append(l,r) if (y.size - x.size) >
                            (r.size - x.size).abs =>
          val l2 = x ++ l
          if (force || r.size*2 > l2.size) Append(l2, r)
          else go(l2, r, force = true)
        case _ => Append(x, y)
      }
    if (other.isEmpty) this
    else this match {
      case b@Buffer(_,_,_) => b.snoc(other)
      case _ =>
        if (this.isEmpty) other
        else go(this, other, false)
    }
  }

  /**
   * Returns a new vector with the specified byte prepended.
   * @group collection
   */
  final def +:(byte: Byte): ByteVector = ByteVector(byte) ++ this

  /**
   * Returns a new vector with the specified byte appended.
   * @group collection
   */
  def :+(byte: Byte): ByteVector =
    this ++ ByteVector(byte)

  /**
   * Returns a vector of all bytes in this vector except the first `n` bytes.
   *
   * The resulting vector's size is `0 max (size - n)`.
   *
   * @group collection
   */
  final def drop(n: Int): ByteVector = {
    val n1 = n min size max 0
    if (n1 == size) ByteVector.empty
    else if (n1 == 0) this
    else {
      @annotation.tailrec
      def go(cur: ByteVector, n1: Int, accR: List[ByteVector]): ByteVector =
        cur match {
          case Chunk(bs) => accR.foldLeft(Chunk(bs.drop(n1)): ByteVector)(_ ++ _)
          case Append(l,r) => if (n1 > l.size) go(r, n1-l.size, accR)
                              else go(l, n1, r :: accR)
          case b@Buffer(_,_,_) => b.drop(n1, accR)
        }
      go(this, n1, Nil)
    }
  }

  /**
   * Returns a vector of all bytes in this vector except the last `n` bytes.
   *
   * The resulting vector's size is `0 max (size - n)`.
   *
   * @group collection
   */
  final def dropRight(n: Int): ByteVector =
    take(size - n.max(0))

  /**
   * Returns a vector of the first `n` bytes of this vector.
   *
   * The resulting vector's size is `n min size`.
   *
   * Note: if an `n`-bit vector is required, use the `acquire` method instead.
   *
   * @see acquire
   * @group collection
   */
  final def take(n: Int): ByteVector = {
    val n1 = n min size max 0
    if (n1 == size) this
    else if (n1 == 0) ByteVector.empty
    else {
      def go(accL: List[ByteVector], cur: ByteVector, n1: Int): ByteVector = cur match {
        case Chunk(bs) => accL.foldLeft(Chunk(bs.take(n1)): ByteVector)((r,l) => l ++ r)
        case Append(l,r) => if (n1 > l.size) go(l :: accL, r, n1-l.size)
                            else go(accL, l, n1)
        case b@Buffer(_,_,_) => b.take(accL, n1)
      }
      go(Nil, this, n1)
    }
  }

  /**
   * Returns a vector of the last `n` bytes of this vector.
   *
   * The resulting vector's size is `n min size`.
   *
   * @group collection
   */
  final def takeRight(n: Int): ByteVector =
    drop(size - n)

  /**
   * Returns a pair of vectors that is equal to `(take(n), drop(n))`.
   * @group collection
   */
  final def splitAt(n: Int): (ByteVector, ByteVector) = (take(n), drop(n))

  /**
   * Returns a vector made up of the bytes starting at index `from` up to index `until`.
   * @group collection
   */
  final def slice(from: Int, until: Int): ByteVector =
    drop(from).take(until - from)

  /**
   * Returns a vector whose contents are the results of taking the first `n` bytes of this vector.
   *
   * If this vector does not contain at least `n` bytes, an error message is returned.
   *
   * @see take
   * @group collection
   */
  def acquire(n: Int): Either[String, ByteVector] =
    if (n <= size) Right(take(n))
    else Left(s"cannot acquire $n bytes from a vector that contains $size bytes")

  /**
   * Consumes the first `n` bytes of this vector and decodes them with the specified function,
   * resulting in a vector of the remaining bytes and the decoded value. If this vector
   * does not have `n` bytes or an error occurs while decoding, an error is returned instead.
   *
   * @group collection
   */
  final def consume[A](n: Int)(decode: ByteVector => Either[String, A]): Either[String, (ByteVector, A)] =
    for {
      toDecode <- acquire(n).right
      decoded <- decode(toDecode).right
    } yield (drop(n), decoded)

  /**
   * Applies a binary operator to a start value and all elements of this vector, going left to right.
   * @param z starting value
   * @param f operator to apply
   * @group collection
   */
  final def foldLeft[A](z: A)(f: (A, Byte) => A): A = {
    var acc = z
    foreachS { new F1BU { def apply(b: Byte) = { acc = f(acc,b) } } }
    acc
  }

  /**
   * Applies a binary operator to a start value and all elements of this vector, going right to left.
   * @param z starting value
   * @param f operator to apply
   * @group collection
   */
  final def foldRight[A](z: A)(f: (Byte, A) => A): A =
    reverse.foldLeft(z)((tl,h) => f(h,tl))

  /**
   * Applies the specified function to each element of this vector.
   * @group collection
   */
  final def foreach(f: Byte => Unit): Unit = foreachS(new F1BU { def apply(b: Byte) = f(b) })

  private[scodec] final def foreachS(f: F1BU): Unit = foreachV(_.foreach(f))

  private[scodec] final def foreachV(f: View => Unit): Unit = {
    @annotation.tailrec
    def go(rem: List[ByteVector]): Unit = if (!rem.isEmpty) rem.head match {
      case Chunk(bs) => f(bs); go(rem.tail)
      case Append(l,r) => go(l::r::rem.tail)
      case Buffer(_,_,tl) => go(tl.prependChunks(rem.tail))
    }
    go(this::Nil)
  }

  /**
   * Returns true if this byte vector starts with the specified vector.
   * @group collection
   */
  final def startsWith(b: ByteVector): Boolean =
    take(b.size) == b

  /**
   * Returns true if this byte vector ends with the specified vector.
   * @group collection
   */
  final def endsWith(b: ByteVector): Boolean =
    takeRight(b.size) == b

  /**
   * Finds the first index of the specified byte pattern in this vector.
   * @return index of slice or -1 if not found
   * @group collection
   */
  final def indexOfSlice(slice: ByteVector): Int = indexOfSlice(slice, 0)

  /**
   * Finds the first index after `from` of the specified byte pattern in this vector.
   * @return index of slice or -1 if not found
   * @group collection
   */
  final def indexOfSlice(slice: ByteVector, from: Int): Int = {
    @annotation.tailrec
    def go(b: ByteVector, idx: Int): Int = {
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
  final def containsSlice(slice: ByteVector): Boolean = indexOfSlice(slice) >= 0

  /**
   * Converts this vector in to a sequence of `chunkSize`-byte vectors.
   * @group collection
   */
  final def grouped(chunkSize: Int): Stream[ByteVector] =
    if (isEmpty) Stream.empty
    else if (size <= chunkSize) Stream(this)
    else take(chunkSize) #:: drop(chunkSize).grouped(chunkSize)

  /**
   * Returns the first byte of this vector or throws if vector is emtpy.
   * @group collection
   */
  final def head: Byte = apply(0)

  /**
   * Returns the first byte of this vector or `None` if vector is emtpy.
   * @group collection
   */
  final def headOption: Option[Byte] = lift(0)

  /**
   * Returns a vector of all bytes in this vector except the first byte.
   * @group collection
   */
  final def tail: ByteVector = drop(1)

  /**
   * Returns a vector of all bytes in this vector except the last byte.
   * @group collection
   */
  final def init: ByteVector = dropRight(1)

  /**
   * Returns the last byte in this vector or throws if vector is empty.
   * @group collection
   */
  final def last: Byte = apply(size-1)

  /**
   * Returns the last byte in this vector or returns `None` if vector is empty.
   * @group collection
   */
  final def lastOption: Option[Byte] = lift(size-1)

  /**
   * Alias for `padRight`.
   *
   * @throws IllegalArgumentException if `n < size`
   * @group collection
   */
  final def padTo(n: Int): ByteVector = padRight(n)

  /**
   * Returns an `n`-byte vector whose contents are this vector's contents followed by 0 or more zero bytes.
   *
   * @throws IllegalArgumentException if `n < size`
   * @group collection
   */
  final def padRight(n: Int): ByteVector =
    if (n < size) throw new IllegalArgumentException(s"ByteVector.padRight($n)")
    else this ++ ByteVector.fill(n - size)(0)

  /**
   * Returns an `n`-bytes vector whose contents are 0 or more zero bytes followed by this vector's contents.
   *
   * @throws IllegalArgumentException if `n < size`
   * @group collection
   */
  final def padLeft(n: Int): ByteVector =
    if (n < size) throw new IllegalArgumentException(s"ByteVector.padLeft($n)")
    else ByteVector.fill(n - size)(0) ++ this

  /**
   * Returns a vector where each byte is the result of applying the specified function to the corresponding byte in this vector.
   * $returnsView
   * @group collection
   */
  final def map(f: Byte => Byte): ByteVector =
    ByteVector.view((i: Int) => f(apply(i)), size)

  /**
   * Returns a vector where each byte is the result of applying the specified function to the corresponding byte in this vector.
   * Only the least significant byte is used (the three most significant bytes are ignored).
   * $returnsView
   * @group collection
   */
  final def mapI(f: Byte => Int): ByteVector =
    map(f andThen { _.toByte })

  private[scodec] final def mapS(f: F1B): ByteVector =
    ByteVector.view(new At { def apply(i: Int) = f(ByteVector.this(i)) }, size)

  /**
   * Returns a vector with the bytes of this vector in reverse order.
   * $returnsView
   * @group collection
   */
  final def reverse: ByteVector =
    ByteVector.view(i => apply(size - i - 1), size)

  final def shiftLeft(n: Int): ByteVector =
    BitVector(this).shiftLeft(n).toByteVector

  final def shiftRight(n: Int, signExtension: Boolean): ByteVector =
    BitVector(this).shiftRight(n, signExtension).toByteVector

  final def rotateLeft(n: Int): ByteVector =
    BitVector(this).rotateLeft(n).toByteVector

  final def rotateRight(n: Int): ByteVector =
    BitVector(this).rotateRight(n).toByteVector

  /**
   * Returns a vector with the same contents but represented as a single tree node internally.
   *
   * This may involve copying data, but has the advantage that lookups index directly into a single
   * node rather than traversing a logarithmic number of nodes in this tree.
   *
   * Calling this method on an already compacted vector is a no-op.
   *
   * @group collection
   */
  final def compact: ByteVector = this match {
    case Chunk(_) => this
    case _ => this.copy
  }

  /**
   * Invokes `compact` on any subtrees whose size is `<= chunkSize`.
   * @group collection
   */
  final def partialCompact(chunkSize: Int): ByteVector = this match {
    case small if small.size <= chunkSize => small.compact
    case Append(l,r) => Append(l.partialCompact(chunkSize), r.partialCompact(chunkSize))
    case _ => this
  }

  /**
   * Returns a vector with the same contents as this vector but with a single compacted node made up
   * by evaluating all internal nodes and concatenating their values.
   * @group collection
   */
  final def copy: ByteVector = {
    val arr = this.toArray
    Chunk(View(AtArray(arr), 0, size))
  }

  /**
   * Converts the contents of this vector to a byte array.
   *
   * @group conversions
   */
  final def toArray: Array[Byte] = {
    val buf = new Array[Byte](size)
    copyToArray(buf, 0)
    buf
  }

  /**
   * Copies the contents of this vector to array `xs`, beginning at index `start`.
   *
   * @group conversions
   */
  final def copyToArray(xs: Array[Byte], start: Int): Unit = {
    var i = start
    foreachV{ v => v.copyToArray(xs, i); i += v.size }
  }

  /**
   * Copies the contents of this vector to OutputStream `s`.
   *
   * @group conversions
   */
  final def copyToStream(s: OutputStream): Unit =
    foreachV(_.copyToStream(s))

  /**
   * Converts the contents of this vector to an `IndexedSeq`.
   *
   * @group conversions
   */
  final def toIndexedSeq: IndexedSeq[Byte] = new IndexedSeq[Byte] {
    def length = ByteVector.this.size
    def apply(i: Int) = ByteVector.this.apply(i)
  }

  /**
   * Converts the contents of this vector to a `Seq`.
   *
   * @group conversions
   */
  final def toSeq: Seq[Byte] = toIndexedSeq

  /**
   * Converts the contents of this vector to an `Iterable`.
   *
   * @group conversions
   */
  final def toIterable: Iterable[Byte] = toIndexedSeq

  /**
   * Converts the contents of this vector to a bit vector of `size * 8` bits.
   * @group conversions
   */
  final def toBitVector: BitVector = BitVector(this)

  /**
   * Alias for [[toBitVector]].
   * @group conversions
   */
  final def bits: BitVector = toBitVector

  /**
   * Allocate (unobservable) mutable scratch space at the end of this
   * `ByteVector`, which will be used to support fast `:+` and `++`
   * of small vectors. A default chunk size is used.
   *
   * Note that `:+`, `++`, and `drop` on the result of a call to `buffer`
   * are guaranteed to return another buffered `ByteVector`.
   */
  final def buffer: ByteVector = bufferBy(1024)

  /**
   * Allocate (unobservable) mutable scratch space at the end of this
   * `ByteVector`, with chunks of the given size, which will be used to
   * support fast `:+` and `++` of small vectors.
   *
   * Note that `:+`, `++`, and `drop` on the result of a call to `buffer`
   * are guaranteed to return another buffered `ByteVector`.
   */
  final def bufferBy(chunkSize: Int): ByteVector =
    this match {
      case b@Buffer(_,_,tl) => if (tl.lastChunk.length >= chunkSize) b
                               else b.rebuffer(chunkSize)
      case _ => Buffer(new AtomicLong(0), 0, Tail(Queue(this), new Array[Byte](chunkSize), 0, this.size))
    }

  /**
   * Collapse any buffered chunks at the end of this `ByteVector`,
   * resulting in an unbuffered `ByteVector`.
   */
  def unbuffer: ByteVector = this

  /**
   * Represents the contents of this vector as a read-only `java.nio.ByteBuffer`.
   *
   * The returned buffer is read-only with limit set to the minimum number of bytes needed to
   * represent the contents of this vector, position set to zero, and remaining set to the limit.
   *
   * @group conversions
   */
  final def toByteBuffer: ByteBuffer = this match {
    case Chunk(v) => v.asByteBuffer
    case _        => ByteBuffer.wrap(toArray).asReadOnlyBuffer()
  }

  /**
   * Converts the contents of this byte vector to a binary string of `size * 8` digits.
   *
   * @group conversions
   */
  final def toBin: String = toBin(Bases.Alphabets.Binary)

  /**
   * Converts the contents of this byte vector to a binary string of `size * 8` digits.
   *
   * @group conversions
   */
  final def toBin(alphabet: Bases.BinaryAlphabet): String = {
    val bldr = new StringBuilder
    foreachS { new F1BU { def apply(b: Byte) = {
      var n = 7
      while (n >= 0) {
        val idx = 1 & (b >> n)
        bldr.append(alphabet.toChar(idx))
        n -= 1
      }
    }}}
    bldr.toString
  }

  /**
   * Converts the contents of this byte vector to a hexadecimal string of `size * 2` nibbles.
   *
   * @group conversions
   */
  final def toHex: String = toHex(Bases.Alphabets.HexLowercase)

  /**
   * Converts the contents of this byte vector to a hexadecimal string of `size * 2` nibbles.
   *
   * @group conversions
   */
  final def toHex(alphabet: Bases.HexAlphabet): String = {
    val bldr = new StringBuilder
    foreachS { new F1BU { def apply(b: Byte) =
      bldr.append(alphabet.toChar((b >> 4 & 0x0f).toByte)).append(alphabet.toChar((b & 0x0f).toByte))
    }}
    bldr.toString
  }

  /**
   * Converts the contents of this vector to a base 64 string.
   *
   * @group conversions
   */
  final def toBase64: String = toBase64(Bases.Alphabets.Base64)

  /**
   * Converts the contents of this vector to a base 64 string using the specified alphabet.
   *
   * @group conversions
   */
  final def toBase64(alphabet: Bases.Base64Alphabet): String = {
    val bldr = new StringBuilder
    grouped(3) foreach { triple =>
      val paddingBytes = 3 - triple.size
      triple.toBitVector.grouped(6) foreach { group =>
        val idx = group.padTo(8).shiftRight(2, false).toByteVector.head
        bldr.append(alphabet.toChar(idx))
      }
      if (paddingBytes > 0) bldr.append(alphabet.pad.toString * paddingBytes)
    }
    bldr.toString
  }

  /**
   * Converts the contents of this vector to an int.
   *
   * @param signed whether sign extension should be performed
   * @param ordering order bytes should be processed in
   * @throws IllegalArgumentException if size is greater than 32
   * @group conversions
   */
  final def toInt(signed: Boolean = true, ordering: ByteOrdering = ByteOrdering.BigEndian): Int =
    bits.toInt(signed, ordering)

  /**
   * Converts the contents of this vector to an int.
   *
   * @param signed whether sign extension should be performed
   * @param ordering order bytes should be processed in
   * @throws IllegalArgumentException if size is greater than 64
   * @group conversions
   */
  final def toLong(signed: Boolean = true, ordering: ByteOrdering = ByteOrdering.BigEndian): Long =
    bits.toLong(signed, ordering)

  final def not: ByteVector = mapS { new F1B { def apply(b: Byte) = (~b).toByte } }

  final def or(other: ByteVector): ByteVector =
    zipWithS(other)(new F2B { def apply(b: Byte, b2: Byte) = (b | b2).toByte })

  final def and(other: ByteVector): ByteVector =
    zipWithS(other)(new F2B { def apply(b: Byte, b2: Byte) = (b & b2).toByte })

  final def xor(other: ByteVector): ByteVector =
    zipWithS(other)(new F2B { def apply(b: Byte, b2: Byte) = (b ^ b2).toByte })

  /**
   * Returns a new vector where each byte is the result of evaluating the specified function
   * against the bytes of this vector and the specified vector at the corresponding index.
   * The resulting vector has size `this.size min other.size`.
   * $returnsView
   * @group collection
   */
  final def zipWith(other: ByteVector)(f: (Byte,Byte) => Byte): ByteVector =
    zipWithS(other)(new F2B { def apply(b: Byte, b2: Byte) = f(b,b2) })

  private[scodec] final def zipWithS(other: ByteVector)(f: F2B): ByteVector = {
    val at = new At { def apply(i: Int) = f(ByteVector.this(i), other(i)) }
    Chunk(View(at, 0, size min other.size))
  }

  /**
   * Returns a new vector where each byte is the result of evaluating the specified function
   * against the bytes of this vector and the specified vector at the corresponding index.
   * The resulting vector has size `this.size min other.size`.
   * Only the least significant byte is used (the three most significant bytes are ignored).
   * $returnsView
   * @group collection
   */

  final def zipWithI(other: ByteVector)(op: (Byte, Byte) => Int): ByteVector =
    zipWith(other) { case (l, r) => op(l, r).toByte }

  /**
   * Computes a digest of this byte vector.
   * @param algorithm digest algorithm to use
   * @group conversions
   */
  final def digest(algorithm: String): ByteVector = digest(MessageDigest.getInstance(algorithm))

  /**
   * Computes a digest of this byte vector.
   * @param digest digest to use
   * @group conversions
   */
  final def digest(digest: MessageDigest): ByteVector = {
    foreachS { new F1BU { def apply(b: Byte) = digest.update(b) }}
    ByteVector.view(digest.digest)
  }

  // implementation details, Object methods

  /**
   * Calculates the hash code of this vector. The result is cached.
   * @group collection
   */
  override lazy val hashCode = {
    // todo: this could be recomputed more efficiently using the tree structure
    // given an associative hash function
    import util.hashing.MurmurHash3._
    val chunkSize = 1024 * 64
    @annotation.tailrec
    def go(bytes: ByteVector, h: Int): Int = {
      if (bytes.isEmpty) finalizeHash(h, size)
      else go(bytes.drop(chunkSize), mix(h, bytesHash(bytes.take(chunkSize).toArray)))
    }
    go(this, stringHash("ByteVector"))
  }

  /**
   * Returns true if the specified value is a `ByteVector` with the same contents as this vector.
   * @group collection
   */
  override def equals(other: Any) = other match {
    case that: ByteVector => this.size == that.size &&
                             (0 until this.size).forall(i => this(i) == that(i))
    case other => false
  }

  /**
   * Display the size and bytes of this `ByteVector`.
   * For bit vectors beyond a certain size, only a hash of the
   * contents are shown.
   * @group collection
   */
  override def toString: String =
    if (isEmpty) "ByteVector(empty)"
    else if (size < 512) s"ByteVector($size bytes, 0x${toHex})"
    else s"ByteVector($size bytes, #${hashCode})"

  private[scodec] def pretty(prefix: String): String = this match {
    case Append(l,r) => {
      val psub = prefix + "|  "
      prefix + "*" + "\n" +
      l.pretty(psub) + "\n" +
      r.pretty(psub)
    }
    case _ => prefix + (if (size < 16) "0x"+toHex else "#"+hashCode)
  }

  private def checkIndex(n: Int): Unit =
    if (n < 0 || n >= size)
      throw new IndexOutOfBoundsException(s"invalid index: $n for size $size")

  protected final def writeReplace(): AnyRef = new SerializationProxy(toArray)
}

/**
 * Companion for [[ByteVector]].
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
object ByteVector {

  // various specialized function types
  private[scodec] abstract class At {
    def apply(i: Int): Byte
    def asByteBuffer(offset: Int, size: Int): ByteBuffer = {
      val arr = new Array[Byte](size)
      copyToArray(arr, 0, offset, size)
      ByteBuffer.wrap(arr).asReadOnlyBuffer()
    }
    def copyToArray(xs: Array[Byte], start: Int, offset: Int, size: Int): Unit = {
      var i = 0
      while (i < size) {
        xs(start + i) = apply(offset + i)
        i += 1
      }
    }
    def copyToStream(s: OutputStream, offset: Int, size: Int): Unit = {
      var i = 0
      while (i < size) {
        s.write(apply(offset + i))
        i += 1
      }
    }
  }
  private[scodec] abstract class F1B { def apply(b: Byte): Byte }
  private[scodec] abstract class F1BU { def apply(b: Byte): Unit }
  private[scodec] abstract class F2B { def apply(b: Byte, b2: Byte): Byte }

  private val AtEmpty: At =
    new At {
      def apply(i: Int) = throw new IllegalArgumentException("empty view")
      override def asByteBuffer(start: Int, size: Int): ByteBuffer = ByteBuffer.allocate(0).asReadOnlyBuffer()
    }

  private def AtArray(arr: Array[Byte]): At =
    new At {
      def apply(i: Int) = arr(i)

      override def asByteBuffer(start: Int, size: Int): ByteBuffer = {
        val b = ByteBuffer.wrap(arr, start, size).asReadOnlyBuffer()
        if (start == 0 && size == arr.length) b
        else b.slice()
      }

      override def copyToArray(xs: Array[Byte], start: Int, offset: Int, size: Int): Unit =
        System.arraycopy(arr, offset, xs, start, size)

      override def copyToStream(s: OutputStream, offset: Int, size: Int): Unit = {
        s.write(arr, offset, size)
      }
    }

  private def AtByteBuffer(buf: ByteBuffer): At =
    new At {
      def apply(i: Int) = buf.get(i)
      override def copyToArray(xs: Array[Byte], start: Int, offset: Int, size: Int): Unit = {
        val n = buf.duplicate()
        n.position(offset)
        n.get(xs, start, size)
      }

      override def asByteBuffer(offset: Int, size: Int): ByteBuffer = {
        val b = buf.asReadOnlyBuffer()
        if (offset == 0 && b.position() == 0 && size == b.remaining()) b
        else {
          b.position(offset)
          b.limit(offset + size)
          b.slice()
        }
      }
    }

  private def AtFnI(f: Int => Int): At = new At {
    def apply(i: Int) = f(i).toByte
  }

  private[bits] case class View(at: At, offset: Int, size: Int) {
    def apply(n: Int) = at(offset + n)
    def foreach(f: F1BU): Unit = {
      var i = 0
      while (i < size) { f(at(offset+i)); i += 1 }
      ()
    }
    def asByteBuffer: ByteBuffer = at.asByteBuffer(offset, size)
    def copyToStream(s: OutputStream): Unit =
      at.copyToStream(s, offset, size)
    def copyToArray(xs: Array[Byte], start: Int): Unit =
      at.copyToArray(xs, start, offset, size)
    def take(n: Int): View =
      if (n <= 0) View.empty
      else if (n >= size) this
      else View(at, offset, n)
    def drop(n: Int): View =
      if (n <= 0) this
      else if (n >= size) View.empty
      else View(at, offset+n, size-n)
  }

  private[bits] object View {
    val empty = View(AtEmpty, 0, 0)
  }

  private[bits] case class Chunk(bytes: View) extends ByteVector {
    def size = bytes.size
  }

  private[bits] case class Append(left: ByteVector, right: ByteVector) extends ByteVector {
    lazy val size = left.size + right.size
  }

  /**
   * Empty byte vector.
   * @group constants
   */
  val empty: ByteVector = Chunk(View(AtEmpty, 0, 0))

  /**
   * Constructs a `ByteVector` from a list of literal bytes. Only the least significant
   * byte is used of each integral value.
   * @group constructors
   */
  def apply[A: Integral](bytes: A*): ByteVector = {
    val integral = implicitly[Integral[A]]
    val buf = new Array[Byte](bytes.size)
    var i = 0
    bytes.foreach { b =>
      buf(i) = integral.toInt(b).toByte
      i += 1
    }
    view(buf)
  }

  /**
   * Constructs a `ByteVector` from a collection of bytes.
   * @group constructors
   */
  def apply(bytes: Vector[Byte]): ByteVector = view(bytes, bytes.size)

  /**
   * Constructs a `ByteVector` from an `Array[Byte]`. The given `Array[Byte]` is
   * is copied to ensure the resulting `ByteVector` is immutable.
   * If this is not desired, use `[[ByteVector.view]]`.
   * @group constructors
   */
  def apply(bytes: Array[Byte]): ByteVector = {
    val copy: Array[Byte] = bytes.clone()
    view(copy)
  }

  /**
   * Constructs a `ByteVector` from a `ByteBuffer`. The given `ByteBuffer` is
   * is copied to ensure the resulting `ByteVector` is immutable.
   * If this is not desired, use `[[ByteVector.view]]`.
   * @group constructors
   */
  def apply(buffer: ByteBuffer): ByteVector = {
    val c = buffer.duplicate()
    val arr = Array.ofDim[Byte](c.remaining)
    c.get(arr)
    view(arr)
  }

  /**
   * Constructs a `ByteVector` from a `scala.collection` source of bytes.
   * @group constructors
   */
  def apply(bs: GenTraversableOnce[Byte]): ByteVector =
    view(bs.toArray[Byte])

  /**
   * Constructs a `ByteVector` from an `Array[Byte]`. Unlike `apply`, this
   * does not make a copy of the input array, so callers should take care
   * not to modify the contents of the array passed to this function.
   * @group constructors
   */
  def view(bytes: Array[Byte]): ByteVector =
    Chunk(View(AtArray(bytes), 0, bytes.length))

  /**
   * Constructs a `ByteVector` from a `ByteBuffer`. Unlike `apply`, this
   * does not make a copy of the input buffer, so callers should take care
   * not to modify the contents of the buffer passed to this function.
   * @group constructors
   */
  def view(bytes: ByteBuffer): ByteVector =
    Chunk(View(AtByteBuffer(bytes.slice()), 0, bytes.limit))

  /**
   * Constructs a `ByteVector` from a function from `Int => Byte` and a size.
   * @group constructors
   */
  def view(at: Int => Byte, size: Int): ByteVector =
    Chunk(View(new At { def apply(i: Int) = at(i) }, 0, size))

  /**
   * Constructs a `ByteVector` from a function from `Int => Byte` and a size.
   */
  private[scodec] def view(at: At, size: Int): ByteVector =
    Chunk(View(at, 0, size))

  /**
   * Constructs a `ByteVector` from a function from `Int => Int` and a size,
   * where the `Int` returned by `at` must fit in a `Byte`.
   * @group constructors
   */
  def viewI(at: Int => Int, size: Int): ByteVector =
    Chunk(View(new At { def apply(i: Int) = at(i).toByte }, 0, size))

  /**
   * Constructs a `ByteVector` of the given size, where all bytes have the value `b`.
   * @group constructors
   */
  def fill[A: Integral](size: Int)(b: A): ByteVector = {
    val integral = implicitly[Integral[A]]
    val v = integral.toInt(b).toByte
    view(Array.fill(size)(v))
  }

  /**
   * Constructs a `ByteVector` of the given size, where all bytes have the value `0`.
   * @group constructors
   */
  def low(size: Int): ByteVector = fill(size)(0)

  /**
   * Constructs a `ByteVector` of the given size, where all bytes have the value `0xff`.
   * @group constructors
   */
  def high(size: Int): ByteVector = fill(size)(0xff)

  /**
   * Constructs a bit vector with the 2's complement encoding of the specified value.
   * @param i value to encode
   * @param size size of vector (<= 4)
   * @param ordering byte ordering of vector
   */
  def fromInt(i: Int, size: Int = 4, ordering: ByteOrdering = ByteOrdering.BigEndian): ByteVector =
    BitVector.fromInt(i, size * 8, ordering).bytes

  /**
   * Constructs a bit vector with the 2's complement encoding of the specified value.
   * @param l value to encode
   * @param size size of vector (<= 8)
   * @param ordering byte ordering of vector
   */
  def fromLong(l: Long, size: Int = 8, ordering: ByteOrdering = ByteOrdering.BigEndian): ByteVector =
    BitVector.fromLong(l, size * 8, ordering).bytes

  /**
   * Constructs a `ByteVector` from a hexadecimal string or returns an error message if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   * @group base
   */
  def fromHexDescriptive(str: String, alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase): Either[String, ByteVector] =
    fromHexInternal(str, alphabet).right.map { case (res, _) => res }

  private[bits] def fromHexInternal(str: String, alphabet: Bases.HexAlphabet): Either[String, (ByteVector, Int)] = {
    val prefixed = (str startsWith "0x") || (str startsWith "0X")
    val withoutPrefix = if (prefixed) str.substring(2) else str
    var idx, hi, count = 0
    var midByte = false
    var err: String = null
    val bldr = Vector.newBuilder[Byte]
    while (idx < withoutPrefix.length && (err eq null)) {
      val c = withoutPrefix(idx)
      if (!alphabet.ignore(c)) {
        try {
          val nibble = alphabet.toIndex(c)
          if (midByte) {
            bldr += (hi | nibble).toByte
            midByte = false
          } else {
            hi = (nibble << 4).toByte
            midByte = true
          }
          count += 1
        } catch {
          case e: IllegalArgumentException =>
            err = s"Invalid hexadecimal character '$c' at index ${idx + (if (prefixed) 2 else 0)}"
        }
      }
      idx += 1
    }
    if (err eq null) {
      Right(
        (if (midByte) {
          bldr += hi.toByte
          val result = bldr.result
          ByteVector(result).shiftRight(4, false)
        } else ByteVector(bldr.result), count)
      )
    } else Left(err)
  }

  /**
   * Constructs a `ByteVector` from a hexadecimal string or returns `None` if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   * @group base
   */
  def fromHex(str: String, alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase): Option[ByteVector] = fromHexDescriptive(str, alphabet).right.toOption

  /**
   * Constructs a `ByteVector` from a hexadecimal string or throws an IllegalArgumentException if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   *
   * @throws IllegalArgumentException if the string is not valid hexadecimal
   * @group base
   */
  def fromValidHex(str: String, alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase): ByteVector =
    fromHexDescriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)


  /**
   * Constructs a `ByteVector` from a binary string or returns an error message if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   * @group base
   */
  def fromBinDescriptive(str: String, alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary): Either[String, ByteVector] = fromBinInternal(str, alphabet).right.map { case (res, _) => res }

  private[bits] def fromBinInternal(str: String, alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary): Either[String, (ByteVector, Int)] = {
    val prefixed = (str startsWith "0b") || (str startsWith "0B")
    val withoutPrefix = if (prefixed) str.substring(2) else str
    var idx, byte, bits, count = 0
    var err: String = null
    val bldr = Vector.newBuilder[Byte]
    while (idx < withoutPrefix.length && (err eq null)) {
      val c = withoutPrefix(idx)
      if (!alphabet.ignore(c)) {
        try {
          byte = (byte << 1) | (1 & alphabet.toIndex(c))
          bits += 1
          count += 1
        } catch {
          case e: IllegalArgumentException =>
            err = s"Invalid binary character '$c' at index ${idx + (if (prefixed) 2 else 0)}"
        }
      }
      if (bits == 8) {
        bldr += byte.toByte
        byte = 0
        bits = 0
      }
      idx += 1
    }
    if (err eq null) {
      Right((if (bits > 0) {
        bldr += (byte << (8 - bits)).toByte
        ByteVector(bldr.result).shiftRight(8 - bits, false)
      } else {
        ByteVector(bldr.result)
      }, count))
    } else Left(err)
  }

  /**
   * Constructs a `ByteVector` from a binary string or returns `None` if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   * @group base
   */
  def fromBin(str: String, alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary): Option[ByteVector] = fromBinDescriptive(str, alphabet).right.toOption

  /**
   * Constructs a `ByteVector` from a binary string or throws an IllegalArgumentException if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   *
   * @throws IllegalArgumentException if the string is not valid binary
   * @group base
   */
  def fromValidBin(str: String, alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary): ByteVector =
    fromBinDescriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)

  /**
   * Constructs a `ByteVector` from a base 64 string or returns an error message if the string is not valid base 64.
   *
   * The string may contain whitespace characters.
   * @group base
   */
  def fromBase64Descriptive(str: String, alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64): Either[String, ByteVector] = {
    val Pad = alphabet.pad
    var idx, padding = 0
    var err: String = null
    var acc: BitVector = BitVector.empty
    while (idx < str.length && (err eq null)) {
      val c = str(idx)
      if (padding == 0) {
        c match {
          case c if alphabet.ignore(c) => // ignore
          case Pad => padding += 1
          case _ =>
            try acc = acc ++ BitVector(alphabet.toIndex(c)).drop(2)
            catch {
              case e: IllegalArgumentException => err = s"Invalid base 64 character '$c' at index $idx"
            }
        }
      } else {
        c match {
          case c if alphabet.ignore(c) => // ignore
          case Pad => padding += 1
          case other => err = s"Unexpected character '$other' at index $idx after padding character; only '=' and whitespace characters allowed after first padding character"
        }
      }
      idx += 1
    }
    if (err eq null) {
      Right(acc.take(acc.size / 8 * 8).toByteVector)
    } else Left(err)
  }

  /**
   * Constructs a `ByteVector` from a base 64 string or returns `None` if the string is not valid base 64.
   *
   * The string may contain whitespace characters.
   * @group base
   */
  def fromBase64(str: String, alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64): Option[ByteVector] = fromBase64Descriptive(str, alphabet).right.toOption

  /**
   * Constructs a `ByteVector` from a base 64 string or throws an IllegalArgumentException if the string is not valid base 64.
   *
   * The string may contain whitespace characters.
   *
   * @throws IllegalArgumentException if the string is not valid base 64
   * @group base
   */
  def fromValidBase64(str: String, alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64): ByteVector =
    fromBase64Descriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)

  @SerialVersionUID(1L)
  private class SerializationProxy(private val bytes: Array[Byte]) extends Serializable {
    def readResolve: AnyRef = ByteVector.view(bytes)
  }

  /*
   * `Buffer` is a `ByteVector` supporting fast `:+` and `++` operations, using
   * an (unobservable) mutable scratch `Array[Byte]` at the end of the vector.
   * The scratch space is written in an append-only fashion, which lets us
   * safely share the same `Array` between multiple `ByteVector` instances,
   * so long as we mutably append to this scratch space _at most once_ from a
   * given `ByteVector`.
   *
   * That is, given {{{
   *    val b = ByteVector.empty.buffer
   *    val b2 = b ++ foo
   *    val b3 = b ++ bar
   *    val b22 = b2 ++ baz
   *  }}}
   *
   * we obviously cannot have both `b2` and `b3` mutably append to the same scratch
   * space, as `b3` would then overwrite what was written by `b2`. The solution
   * adopted here (see [1]) is to let `b2` write mutable to the buffer, but `b3`,
   * evaluated later, must make a copy.
   *
   * [1]: http://www.serpentine.com/blog/2014/05/31/attoparsec/
   */
  private case class Buffer(id: AtomicLong, stamp: Long, tl: Tail) extends ByteVector {
    def size = tl.size

    override def :+(b: Byte) =
      snoc(b)

    // NB: if `bs` fits in scratch space and is itself a buffer, it will be
    // unbuffered before being added. This avoids proliferation of scratch space
    // for tiny ByteVectors
    def snoc(bs: ByteVector): ByteVector =
      // threads race to increment id, winner gets to update tl mutably
      if (id.compareAndSet(stamp, stamp+1))
        Buffer(id, stamp+1, tl.snoc(bs))
      else // loser has to make a copy of the scratch space
        Buffer(new AtomicLong(0), 0, tl.freshen).snoc(bs)

    def snoc(b: Byte): ByteVector =
      // threads race to increment id, winner gets to update tl mutably
      if (id.compareAndSet(stamp, stamp+1))
        Buffer(id, stamp+1, tl.snoc(b))
      else // loser has to make a copy of the scratch space
        Buffer(new AtomicLong(0), 0, tl.freshen).snoc(b)

    override def unbuffer: ByteVector = tl.toByteVector

    def rebuffer(chunkSize: Int): ByteVector =
      Buffer(new AtomicLong(0), 0, tl.rebuffer(chunkSize))

    def drop(n: Int, accR: List[ByteVector]): ByteVector =
      accR.foldLeft(tl.drop(id, stamp, n))(_ ++ _)

    def take(accL: List[ByteVector], n: Int): ByteVector =
      accL.foldLeft(tl.take(id, stamp, n))((r,l) => l ++ r)
  }

  private case class Tail(chunks: Queue[ByteVector], lastChunk: Array[Byte], lastSize: Int, size: Int) {

    // make a copy of the last chunk
    def freshen: Tail = {
      val lastChunk2 = new Array[Byte](lastChunk.length)
      lastChunk.copyToArray(lastChunk2)
      Tail(chunks, lastChunk2, lastSize, size)
    }

    def rebuffer(chunkSize: Int): Tail = {
      require (chunkSize > lastChunk.length)
      val lastChunk2 = new Array[Byte](chunkSize)
      lastChunk.copyToArray(lastChunk2)
      Tail(chunks, lastChunk2, lastSize, size)
    }

    def drop(id: AtomicLong, stamp: Long, n: Int): ByteVector =
      if (n > lastChunk.length * 3) toByteVector.drop(n)
      else { // if dropping just a few chunks' worth, just dequeue necessary elements from `chunks`
        var remaining = n
        var q = chunks
        while (remaining > 0 && q.nonEmpty) {
          val (chunk,q2) = q.dequeue
          if (chunk.size > remaining) {
            q = chunk.drop(remaining) +: q2
            remaining = 0
          }
          else {
            remaining -= chunk.size
            q = q2
          }
        }
        if (remaining > 0) // we've exhaused `chunks`, take from `lastChunk`
          // we still rebuffer, useful to have invariant that dropping from a `Buffer` always yields a `Buffer`
          ByteVector.view(lastChunk).take(lastSize).drop(remaining).bufferBy(lastChunk.length)
        else // we've already dropped everything we need to, build new `Buffer`
          Buffer(id, stamp, Tail(q, lastChunk, lastSize, size - n))
      }

    def take(id: AtomicLong, stamp: Long, n: Int): ByteVector =
      if (n < lastChunk.length * 3) { // if taking just a few chunks' worth, dequeue necessary elements from `chunks`
        var remaining = n
        var q = chunks enqueue ByteVector.view(lastChunk).take(lastSize)
        var acc = ByteVector.empty
        while (remaining > 0) {
          val (chunk,q2) = q.dequeue
          if (chunk.size > remaining) {
            acc = acc ++ chunk.take(remaining)
            remaining = 0
          }
          else {
            remaining -= chunk.size
            acc = acc ++ chunk
            q = q2
          }
        }
        acc
      }
      else // otherwise, convert to full `ByteVector`
        toByteVector.take(n)

    final def snoc(bs: ByteVector): Tail =
      if (bs.size >= lastChunk.length) {
        if (lastSize != 0) { // we promote `lastChunk` to `chunks`
          val buf = new Array[Byte](lastChunk.length)
          Tail(chunks.enqueue(ByteVector.view(lastChunk).take(lastSize)).enqueue(bs), buf, 0, size + bs.size)
        }
        else // just enqueue `bs` directly to `chunks`
          Tail(chunks.enqueue(bs), lastChunk, lastSize, size + bs.size)
      }
      else { // try filling up `lastChunk`
        val rem = lastChunk.length - lastSize
        if (bs.size <= rem) {
          bs.copyToArray(lastChunk, lastSize)
          Tail(chunks, lastChunk, lastSize + bs.size, size + bs.size)
        }
        else {
          bs.take(rem).copyToArray(lastChunk, lastSize)
          val chunks2 = chunks enqueue ByteVector.view(lastChunk)
          val buf = new Array[Byte](lastChunk.length)
          val bst = bs.drop(rem)
          bst.copyToArray(buf, 0)
          Tail(chunks2, buf, bst.size, size + bs.size)
        }
      }

    def snoc(b: Byte): Tail =
      if (lastSize == lastChunk.length) { // last chunk is full, move it to `chunks`
        val lastChunk2 = new Array[Byte](lastChunk.length)
        lastChunk2(0) = b // add the byte to a freshly-allocated array
        Tail(chunks.enqueue(ByteVector.view(lastChunk)), lastChunk2, 1, size+1)
      }
      else { // last chunk not full, set the byte and increment lastSize
        lastChunk(lastSize) = b
        Tail(chunks, lastChunk, lastSize + 1, size + 1)
      }

    def get(i: Int): Byte =
      toByteVector.get(i)

    def prependChunks(tl: List[ByteVector]): List[ByteVector] =
      if (lastSize == 0) chunks.toList ++ tl
      else chunks.toList ++ (ByteVector.view(lastChunk).take(lastSize) :: tl)

    // we cache this to get reasonable performance in case this buffer gets used
    // as a regular vector, with repeated calls to `get`, `take`, `drop`, etc
    lazy val toByteVector =
      chunks.foldLeft(ByteVector.empty)(_ ++ _) ++ ByteVector.view(lastChunk).take(lastSize)
  }
}

