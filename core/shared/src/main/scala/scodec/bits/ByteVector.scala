package scodec.bits

import ByteVector._

import java.io.OutputStream
import java.nio.{ ByteBuffer, CharBuffer }
import java.nio.charset.{ CharacterCodingException, Charset }
import java.security.{ AlgorithmParameters, GeneralSecurityException, Key, MessageDigest, SecureRandom }
import java.util.concurrent.atomic.AtomicLong
import java.util.zip.{ DataFormatException, Deflater, Inflater }
import javax.crypto.Cipher

import scala.collection.GenTraversableOnce
import collection.immutable.Queue

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
 * @groupname buffer Buffering
 * @groupprio buffer 3
 *
 * @groupname crypto Cryptography
 * @groupprio crypto 4
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
  def get(index: Int): Byte = {
    checkIndex(index)
    getImpl(index)
  }

  protected def getImpl(index: Int): Byte

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
  def ++(other: ByteVector): ByteVector =
    if (this.isEmpty) other
    else Chunks(Append(this, other)).bufferBy(64)

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
  def drop(n: Int): ByteVector = {
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
          case b: Buffer => if (n1 > b.hd.size) go(b.lastBytes, n1-b.hd.size, accR)
                            else go(b.hd, n1, b.lastBytes :: accR)
          case c: Chunks => go(c.chunks, n1, accR)
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
   * Drops the longest prefix of this vector such that every byte of the prefix satisfies the specific predicate.
   *
   * @group collection
   */
  final def dropWhile(f: Byte => Boolean): ByteVector = {
    var toDrop = 0
    foreachSPartial(new F1BB { def apply(b: Byte) = {
      val cont = f(b)
      if (cont) toDrop += 1
      cont
    }})
    drop(toDrop)
  }

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
  def take(n: Int): ByteVector = {
    val n1 = n min size max 0
    if (n1 == size) this
    else if (n1 == 0) ByteVector.empty
    else {
      @annotation.tailrec
      def go(accL: ByteVector, cur: ByteVector, n1: Int): ByteVector = cur match {
        case Chunk(bs) => accL ++ Chunk(bs.take(n1))
        case Append(l,r) =>
          if (n1 > l.size) go(accL ++ l, r, n1 - l.size)
          else go(accL, l, n1)
        case c: Chunks => go(accL, c.chunks, n1)
        case b: Buffer => go(accL, b.unbuffer, n1)
      }
      go(ByteVector.empty, this, n1)
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
   * Returns the longest prefix of this vector such that every byte satisfies the specific predicate.
   *
   * @group collection
   */
  final def takeWhile(f: Byte => Boolean): ByteVector = {
    var toTake = 0
    foreachSPartial(new F1BB { def apply(b: Byte) = {
      val cont = f(b)
      if (cont) toTake += 1
      cont
    }})
    take(toTake)
  }

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

  private[scodec] final def foreachSPartial(f: F1BB): Boolean = foreachVPartial(_.foreachPartial(f))

  private[scodec] final def foreachV(f: View => Unit): Unit = {
    @annotation.tailrec
    def go(rem: List[ByteVector]): Unit = rem match {
      case Chunk(bs)   :: rem => f(bs); go(rem)
      case Append(l,r) :: rem => go(l :: r :: rem)
      case Chunks(Append(l,r)) :: rem => go(l :: r :: rem)
      case (b: Buffer) :: rem => go(b.unbuffer :: rem)
      case Nil => ()
    }
    go(this::Nil)
  }

  private[scodec] final def foreachVPartial(f: View => Boolean): Boolean = {
    @annotation.tailrec
    def go(rem: List[ByteVector]): Boolean = rem match {
      case Chunk(bs)   :: rem => if (f(bs)) go(rem) else false
      case Append(l,r) :: rem => go(l :: r :: rem)
      case Chunks(Append(l,r)) :: rem => go(l :: r :: rem)
      case (b: Buffer) :: rem => go(b.unbuffer :: rem)
      case Nil => true
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
    BitVector(this).shiftLeft(n.toLong).toByteVector

  final def shiftRight(n: Int, signExtension: Boolean): ByteVector =
    BitVector(this).shiftRight(n.toLong, signExtension).toByteVector

  final def rotateLeft(n: Int): ByteVector =
    BitVector(this).rotateLeft(n.toLong).toByteVector

  final def rotateRight(n: Int): ByteVector =
    BitVector(this).rotateRight(n.toLong).toByteVector

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
    foreachV { v => v.copyToArray(xs, i); i += v.size }
  }

  /**
   * Copies `size` bytes of this vector, starting at index `offset`, to array `xs`, beginning at index `start`.
   *
   * @group conversions
   */
  final def copyToArray(xs: Array[Byte], start: Int, offset: Int, size: Int): Unit = {
    var i = 0
    var voffset = 0
    foreachV { v =>
      if (i < size) {
        val reloff = (offset - voffset) max 0
        if (voffset >= offset || reloff < v.size) {
          val sz = (size - i) min (v.size - reloff)
          v.copyToArray(xs, start + i, reloff, sz)
          i += sz
        }
        voffset += v.size
      }
    }
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
   *
   * @group buffer
   */
  final def buffer: ByteVector = bufferBy(1024)

  /**
   * Allocate (unobservable) mutable scratch space at the end of this
   * `ByteVector`, with chunks of the given size, which will be used to
   * support fast `:+` and `++` of small vectors.
   *
   * Note that `:+`, `++`, and `drop` on the result of a call to `buffer`
   * are guaranteed to return another buffered `ByteVector`, with the
   * same size scratch space.
   *
   * @group buffer
   */
  final def bufferBy(chunkSize: Int): ByteVector =
    this match {
      case b: Buffer => if (b.lastChunk.length >= chunkSize) b
                        else b.rebuffer(chunkSize)
      case _ => Buffer(new AtomicLong(0), 0, this, new Array[Byte](chunkSize), 0)
    }

  /**
   * Collapse any buffered chunks at the end of this `ByteVector`,
   * resulting in an unbuffered `ByteVector`.
   *
   * @group buffer
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
    foreachS { new F1BU { def apply(b: Byte) = {
      bldr.append(alphabet.toChar((b >> 4 & 0x0f).toByte.toInt)).append(alphabet.toChar((b & 0x0f).toByte.toInt))
      ()
    }}}
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
    val bytes = toArray
    val bldr = CharBuffer.allocate(((bytes.length + 2) / 3) * 4)
    var idx = 0
    val mod = bytes.length % 3
    while (idx < bytes.length - mod) {
      var buffer = ((bytes(idx) & 0x0ff) << 16) | ((bytes(idx + 1) & 0x0ff) << 8) | (bytes(idx + 2) & 0x0ff)
      val fourth = buffer & 0x3f
      buffer = buffer >> 6
      val third = buffer & 0x3f
      buffer = buffer >> 6
      val second = buffer & 0x3f
      buffer = buffer >> 6
      val first = buffer
      bldr.append(alphabet.toChar(first)).append(alphabet.toChar(second)).append(alphabet.toChar(third)).append(alphabet.toChar(fourth))
      idx = idx + 3
    }
    if (mod == 1) {
      var buffer = (bytes(idx) & 0x0ff) << 4
      val second = buffer & 0x3f
      buffer = buffer >> 6
      val first = buffer
      bldr.append(alphabet.toChar(first)).append(alphabet.toChar(second)).append(alphabet.pad).append(alphabet.pad)
    } else if (mod == 2) {
      var buffer = ((bytes(idx) & 0x0ff) << 10) | ((bytes(idx + 1) & 0x0ff) << 2)
      val third = buffer & 0x3f
      buffer = buffer >> 6
      val second = buffer & 0x3f
      buffer = buffer >> 6
      val first = buffer
      bldr.append(alphabet.toChar(first)).append(alphabet.toChar(second)).append(alphabet.toChar(third)).append(alphabet.pad)
    }
    bldr.flip.toString
  }

  /**
   * Converts the contents of this vector to a byte.
   *
   * @param signed whether sign extension should be performed
   * @throws IllegalArgumentException if size is greater than 8
   * @group conversions
   */
  final def toByte(signed: Boolean = true): Byte =
    bits.toByte(signed)

  /**
   * Converts the contents of this vector to a short.
   *
   * @param signed whether sign extension should be performed
   * @param ordering order bytes should be processed in
   * @throws IllegalArgumentException if size is greater than 16
   * @group conversions
   */
  final def toShort(signed: Boolean = true, ordering: ByteOrdering = ByteOrdering.BigEndian): Short =
    bits.toShort(signed, ordering)

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

  /**
   * Decodes this vector as a string using the implicitly available charset.
   * @group conversions
   */
  final def decodeString(implicit charset: Charset): Either[CharacterCodingException, String] = {
    val decoder = charset.newDecoder
    try {
      Right(decoder.decode(toByteBuffer).toString)
    } catch {
      case e: CharacterCodingException => Left(e)
    }
  }

  /**
   * Decodes this vector as a string using the UTF-8 charset.
   * @group conversions
   */
  final def decodeUtf8: Either[CharacterCodingException, String] =
    decodeString(Charset.forName("UTF-8"))

  /**
   * Decodes this vector as a string using the US-ASCII charset.
   * @group conversions
   */
  final def decodeAscii: Either[CharacterCodingException, String] =
    decodeString(Charset.forName("US-ASCII"))

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
   * Compresses this vector using ZLIB.
   *
   * @param level compression level, 0-9, with 0 disabling compression and 9 being highest level of compression -- see `java.util.zip.Deflater` for details
   * @param strategy compression strategy -- see `java.util.zip.Deflater` for details
   * @param nowrap if true, ZLIB header and checksum will not be used
   * @param chunkSize buffer size, in bytes, to use when compressing
   * @group conversions
   */
  final def deflate(level: Int = Deflater.DEFAULT_COMPRESSION, strategy: Int = Deflater.DEFAULT_STRATEGY, nowrap: Boolean = false, chunkSize: Int = 4096): ByteVector = {
    if (isEmpty) this
    else {
      val deflater = new Deflater(level, nowrap)
      try {
        deflater.setStrategy(strategy)

        val buffer = new Array[Byte](chunkSize min size)
        def loop(acc: ByteVector, fin: Boolean): ByteVector = {
          if ((fin && deflater.finished) || (!fin && deflater.needsInput)) acc
          else {
            val count = deflater deflate buffer
            loop(acc ++ ByteVector(buffer, 0, count), fin)
          }
        }

        var result = ByteVector.empty

        foreachV { v =>
          deflater.setInput(v.toArray)
          result = result ++ loop(ByteVector.empty, false)
        }

        deflater.setInput(Array.empty)
        deflater.finish()
        result ++ loop(ByteVector.empty, true)
      } finally {
        deflater.end()
      }
    }
  }

  /**
   * Decompresses this vector using ZLIB.
   *
   * @param chunkSize buffer size, in bytes, to use when compressing
   * @group conversions
   */
  final def inflate(chunkSize: Int = 4096): Either[DataFormatException, ByteVector] = {
    if (isEmpty) Right(this)
    else {
      val arr = toArray

      val inflater = new Inflater
      try {
        inflater.setInput(arr)
        try {
          val buffer = new Array[Byte](chunkSize min arr.length)
          def loop(acc: ByteVector): ByteVector = {
            if (inflater.finished || inflater.needsInput) acc
            else {
              val count = inflater inflate buffer
              loop(acc ++ ByteVector(buffer, 0, count))
            }
          }
          val inflated = loop(ByteVector.empty)
          if (inflater.finished) Right(inflated)
          else Left(new DataFormatException("Insufficient data -- inflation reached end of input without completing inflation - " + inflated))
        } catch {
          case e: DataFormatException => Left(e)
        }
      } finally {
        inflater.end()
      }
    }
  }

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
    foreachV { v => digest.update(v.toArray) }
    ByteVector.view(digest.digest)
  }

  /**
   * Encrypts this byte vector using the specified cipher and key.
   *
   * @param ci cipher to use for encryption
   * @param key key to encrypt with
   * @param aparams optional algorithm paramaters used for encryption (e.g., initialization vector)
   * @param sr secure random
   * @group crypto
   */
  final def encrypt(ci: Cipher, key: Key, aparams: Option[AlgorithmParameters] = None)(implicit sr: SecureRandom): Either[GeneralSecurityException, ByteVector] =
    cipher(ci, key, Cipher.ENCRYPT_MODE, aparams)

  /**
   * Decrypts this byte vector using the specified cipher and key.
   *
   * @param ci cipher to use for decryption
   * @param key key to decrypt with
   * @param aparams optional algorithm paramaters used for decryption (e.g., initialization vector)
   * @param sr secure random
   * @group crypto
   */
  final def decrypt(ci: Cipher, key: Key, aparams: Option[AlgorithmParameters] = None)(implicit sr: SecureRandom): Either[GeneralSecurityException, ByteVector] =
    cipher(ci, key, Cipher.DECRYPT_MODE, aparams)

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

  private[bits] def cipher(ci: Cipher, key: Key, opmode: Int, aparams: Option[AlgorithmParameters] = None)(implicit sr: SecureRandom): Either[GeneralSecurityException, ByteVector] = {
    try {
      aparams.fold(ci.init(opmode, key, sr))(aparams => ci.init(opmode, key, aparams, sr))
      foreachV { view => ci.update(view.asByteBuffer.array); () }
      Right(ByteVector.view(ci.doFinal()))
    } catch {
      case e: GeneralSecurityException => Left(e)
    }
  }

  private[bits] def pretty(prefix: String): String = this match {
    case Append(l,r) =>
      prefix + "bytes:append\n" +
      l.pretty(prefix + "  ") + "\n" +
      r.pretty(prefix + "  ")
    case Chunks(c) =>
      prefix + "bytes:chunks " + size + "\n" +
      c.left.pretty(prefix + "  ") + "\n" +
      c.right.pretty(prefix + "  ")
    case b: Buffer => prefix + "bytes:buffer " + size + "\n" +
                      b.hd.pretty(prefix + "  ") + "\n" +
                      b.lastBytes.pretty(prefix + "  ")
    case Chunk(_) => prefix + (if (size < 16) "0x"+toHex else "#"+hashCode)
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
 * @groupname numeric Numeric Conversions
 * @groupprio numeric 2
 *
 * @groupname base Base Conversions
 * @groupprio base 3
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
        s.write(apply(offset + i).toInt)
        i += 1
      }
    }
  }
  private[scodec] abstract class F1B { def apply(b: Byte): Byte }
  private[scodec] abstract class F1BU { def apply(b: Byte): Unit }
  private[scodec] abstract class F1BB { def apply(b: Byte): Boolean }
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
        ()
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
      while (i < size) {
        f(at(offset + i))
        i += 1
      }
    }
    def foreachPartial(f: F1BB): Boolean = {
      var i = 0
      var cont = true
      while (i < size && cont) {
        cont = f(at(offset + i))
        i += 1
      }
      cont
    }
    def asByteBuffer: ByteBuffer = at.asByteBuffer(offset, size)
    def copyToStream(s: OutputStream): Unit =
      at.copyToStream(s, offset, size)
    def copyToArray(xs: Array[Byte], start: Int): Unit =
      at.copyToArray(xs, start, offset, size)
    def copyToArray(xs: Array[Byte], start: Int, offset: Int, size: Int): Unit =
      at.copyToArray(xs, start, this.offset + offset, this.size min size)
    def toArray: Array[Byte] = {
      val arr = new Array[Byte](size)
      copyToArray(arr, 0)
      arr
    }
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
    def getImpl(i: Int) = bytes(i)
  }

  private[bits] case class Append(left: ByteVector, right: ByteVector) extends ByteVector {
    val size = left.size + right.size
    def getImpl(n: Int) = {
      if (n < left.size) left.getImpl(n)
      else right.getImpl(n - left.size)
    }
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
   * Constructs a `ByteVector` from an `Array[Byte]`. The given `Array[Byte]`
   * is copied to ensure the resulting `ByteVector` is immutable.
   * If this is not desired, use `ByteVector.view`.
   * @group constructors
   */
  def apply(bytes: Array[Byte]): ByteVector = {
    val copy: Array[Byte] = bytes.clone()
    view(copy)
  }

  /**
   * Constructs a `ByteVector` from an `Array[Byte]`, an offset, and a length.
   * The given `Array[Byte]` is copied to ensure the resulting `ByteVector` is immutable.
   * If this is not desired, use `ByteVector.view`.
   * @group constructors
   */
  def apply(bytes: Array[Byte], offset: Int, length: Int): ByteVector = {
    val fresh: Array[Byte] = new Array[Byte](length)
    System.arraycopy(bytes, offset, fresh, 0, length)
    view(fresh)
  }

  /**
   * Constructs a `ByteVector` from a `ByteBuffer`. The given `ByteBuffer` is
   * is copied to ensure the resulting `ByteVector` is immutable.
   * If this is not desired, use `ByteVector.view`.
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
   * Constructs a bit vector with the 2's complement encoding of the specified byte.
   * @param b value to encode
   * @group numeric
   */
  def fromByte(b: Byte): ByteVector =
    BitVector.fromByte(b, 8).bytes

  /**
   * Constructs a bit vector with the 2's complement encoding of the specified value.
   * @param s value to encode
   * @param size size of vector (<= 2)
   * @param ordering byte ordering of vector
   * @group numeric
   */
  def fromShort(s: Short, size: Int = 2, ordering: ByteOrdering = ByteOrdering.BigEndian): ByteVector =
    BitVector.fromShort(s, size * 8, ordering).bytes

  /**
   * Constructs a bit vector with the 2's complement encoding of the specified value.
   * @param i value to encode
   * @param size size of vector (<= 4)
   * @param ordering byte ordering of vector
   * @group numeric
   */
  def fromInt(i: Int, size: Int = 4, ordering: ByteOrdering = ByteOrdering.BigEndian): ByteVector =
    BitVector.fromInt(i, size * 8, ordering).bytes

  /**
   * Constructs a bit vector with the 2's complement encoding of the specified value.
   * @param l value to encode
   * @param size size of vector (<= 8)
   * @param ordering byte ordering of vector
   * @group numeric
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
    val bldr = ByteBuffer.allocate((str.size + 1) / 2)
    while (idx < withoutPrefix.length && (err eq null)) {
      val c = withoutPrefix(idx)
      if (!alphabet.ignore(c)) {
        try {
          val nibble = alphabet.toIndex(c)
          if (midByte) {
            bldr.put((hi | nibble).toByte)
            midByte = false
          } else {
            hi = (nibble << 4).toByte.toInt
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
          bldr.put(hi.toByte)
          bldr.flip()
          ByteVector(bldr).shiftRight(4, false)
        } else {
          bldr.flip()
          ByteVector(bldr)
        }, count)
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
    val bldr = ByteBuffer.allocate((str.size + 7) / 8)
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
        bldr.put(byte.toByte)
        byte = 0
        bits = 0
      }
      idx += 1
    }
    if (err eq null) {
      Right((if (bits > 0) {
        bldr.put((byte << (8 - bits)).toByte)
        bldr.flip()
        ByteVector(bldr).shiftRight(8 - bits, false)
      } else {
        bldr.flip()
        ByteVector(bldr)
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

  /**
   * Encodes the specified string to a `ByteVector` using the implicitly available `Charset`.
   *
   * @group constructors
   */
  def encodeString(str: String)(implicit charset: Charset): Either[CharacterCodingException, ByteVector] = {
    val encoder = charset.newEncoder
    val buffer = CharBuffer.wrap(str)
    try Right(ByteVector(encoder.encode(buffer)))
    catch {
      case e: CharacterCodingException => Left(e)
    }
  }

  /**
   * Encodes the specified string to a `ByteVector` using the UTF-8 charset.
   *
   * @group constructors
   */
  def encodeUtf8(str: String): Either[CharacterCodingException, ByteVector] =
    encodeString(str)(Charset.forName("UTF-8"))

  /**
   * Encodes the specified string to a `ByteVector` using the US-ASCII charset.
   *
   * @group constructors
   */
  def encodeAscii(str: String): Either[CharacterCodingException, ByteVector] =
    encodeString(str)(Charset.forName("US-ASCII"))

  /**
   * Concatenates all the given `ByteVector`s into a single instance.
   *
   * @group constructors
   */
  def concat(bvs: GenTraversableOnce[ByteVector]): ByteVector = bvs.foldLeft(ByteVector.empty)(_ ++ _)

  @SerialVersionUID(1L)
  private class SerializationProxy(private val bytes: Array[Byte]) extends Serializable {
    def readResolve: AnyRef = ByteVector.view(bytes)
  }

  private[bits] case class Chunks(chunks: Append) extends ByteVector {
    def size = chunks.size
    def getImpl(n: Int) = chunks.getImpl(n)

    override def ++(b: ByteVector): ByteVector =
      if (b.isEmpty) this
      else if (this.isEmpty) b
      else {
        @annotation.tailrec
        def go(chunks: Append, last: ByteVector): ByteVector = {
          val lastN = last.size
          if (lastN >= chunks.size || lastN*2 <= chunks.right.size)
            Chunks(Append(chunks, last))
          else chunks.left match {
            case left: Append => go(left, Append(chunks.right, last))
            case _ => Chunks(Append(chunks, last))
          }
        }
        go(chunks, b.unbuffer) // unbuffering `b` avoids proliferation of unused scratch space
                               // if `b` happens to have been built up using a buffer
      }
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
  private case class Buffer(id: AtomicLong, stamp: Long, hd: ByteVector, lastChunk: Array[Byte], lastSize: Int) extends ByteVector {
    def size = hd.size + lastSize

    override def take(n: Int): ByteVector =
      if (n <= hd.size) hd.take(n)
      else hd ++ lastBytes.take(n - hd.size)

    override def drop(n: Int): ByteVector =
      if (n <= hd.size) Buffer(id, stamp, hd.drop(n), lastChunk, lastSize)
      else unbuffer.drop(n).bufferBy(lastChunk.length)

    def getImpl(n: Int): Byte =
      if (n < hd.size) hd.getImpl(n)
      else lastChunk(n - hd.size)

    override def :+(b: Byte): ByteVector =
      // threads race to increment id, winner gets to update tl mutably
      if (id.compareAndSet(stamp, stamp+1) && lastSize < lastChunk.length) {
        lastChunk(lastSize) = b
        Buffer(id, stamp+1, hd, lastChunk, lastSize + 1)
      }
      else // loser has to make a copy of the scratch space
        Buffer(new AtomicLong(0), 0, unbuffer,
               new Array[Byte](lastChunk.length), 0) :+ b

    // NB: if `bs` fits in scratch space and is itself a buffer, it will be
    // unbuffered before being added. This avoids proliferation of scratch space
    // for tiny ByteVectors
    override def ++(bs: ByteVector): ByteVector = if (bs.isEmpty) this else {
      // threads race to increment id, winner gets to update tl mutably
      if (id.compareAndSet(stamp, stamp+1) && (lastChunk.length - lastSize > bs.size)) {
        bs.copyToArray(lastChunk, lastSize)
        Buffer(id, stamp+1, hd, lastChunk, lastSize + bs.size)
      }
      else {
        if (lastSize == 0) // just append directly to `hd`
          Buffer(id, stamp, hd ++ bs, lastChunk, lastSize)
        else // loser has to make a copy of the scratch space
          Buffer(new AtomicLong(0), 0, unbuffer,
                 new Array[Byte](lastChunk.length), 0) ++ bs
      }
    }

    def lastBytes = ByteVector.view(lastChunk).take(lastSize)

    override def unbuffer: ByteVector =
      // if last chunk more than half unused, copy to a fresh
      // bytevector, to avoid proliferation of scratch space
      hd ++ (if (lastSize*2 < lastChunk.length) lastBytes.copy else lastBytes)

    def rebuffer(chunkSize: Int): ByteVector = {
      require (chunkSize > lastChunk.length)
      val lastChunk2 = new Array[Byte](chunkSize)
      lastChunk.copyToArray(lastChunk2)
      Buffer(new AtomicLong(0), 0, hd, lastChunk, lastSize)
    }
  }
}

