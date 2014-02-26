package scodec.bits

import ByteVector._
import java.nio.ByteBuffer
import scala.collection.GenTraversableOnce

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
 * @groupname individual Operations on Individual Bits
 * @groupprio individual 1
 *
 * @groupname bitwise Bitwise Operations
 * @groupprio bitwise 2
 *
 * @groupname conversions Conversions
 * @groupprio conversions 3
 *
 * @define bitwiseOperationsReprDescription bit vector
 */
trait ByteVector extends BitwiseOperations[ByteVector,Int] {

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
    else if (this.isEmpty) other
    else go(this, other, false)
  }

  final def +:(byte: Byte): ByteVector = ByteVector(byte) ++ this

  final def :+(byte: Byte): ByteVector = this ++ ByteVector(byte)

  final def apply(n0: Int): Byte = {
    checkIndex(n0)
    @annotation.tailrec
    def go(cur: ByteVector, n: Int): Byte = cur match {
      case Append(l,r) => if (n < l.size) go(l, n)
                          else go(r, n-l.size)
      case Chunk(arr) => arr(n)
    }
    go(this, n0)
  }

  final def compact: ByteVector = this match {
    case Chunk(_) => this
    case _ => this.copy
  }

  final def copy: ByteVector = {
    val arr = this.toArray
    Chunk(View(AtArray(arr), 0, size))
  }

  final def drop(n: Int): ByteVector = {
    val n1 = n min size max 0
    if (n1 == size) ByteVector.empty
    else if (n1 == 0) this
    else {
      @annotation.tailrec
      def go(cur: ByteVector, n1: Int, accR: Vector[ByteVector]): ByteVector =
        cur match {
          case Chunk(bs) => accR.foldLeft(Chunk(bs.drop(n1)): ByteVector)(_ ++ _)
          case Append(l,r) => if (n1 > l.size) go(r, n1-l.size, accR)
                              else go(l, n1, r +: accR)
        }
      go(this, n1, Vector())
    }
  }

  final def dropRight(n: Int): ByteVector =
    take(size - n.max(0))

  final def foldLeft[A](z: A)(f: (A,Byte) => A): A = {
    var acc = z
    foreachS { new F1BU { def apply(b: Byte) = { acc = f(acc,b) } } }
    acc
  }

  final def foldRight[A](z: A)(f: (Byte,A) => A): A =
    reverse.foldLeft(z)((tl,h) => f(h,tl))

  final def foreach(f: Byte => Unit): Unit = foreachS(new F1BU { def apply(b: Byte) = f(b) })

  private[scodec] final def foreachS(f: F1BU): Unit = {
    @annotation.tailrec
    def go(rem: Vector[ByteVector]): Unit = rem.headOption match {
      case None => ()
      case Some(bytes) => bytes match {
        case Chunk(bs) => bs.foreach(f); go(rem.tail)
        case Append(l,r) => go(l +: r +: rem.tail)
      }
    }
    go(Vector(this))
  }

  final def grouped(chunkSize: Int): Stream[ByteVector] =
    if (isEmpty) Stream.empty
    else if (size <= chunkSize) Stream(this)
    else take(chunkSize) #:: drop(chunkSize).grouped(chunkSize)

  final def head: Byte = apply(0)

  final def headOption: Option[Byte] = lift(0)

  final def init: ByteVector = dropRight(1)

  final def insert(idx: Int, b: Byte): ByteVector =
    (take(idx) :+ b) ++ drop(idx)

  final def isEmpty = size == 0

  final def last: Byte = apply(size-1)
  final def lastOption: Option[Byte] = lift(size-1)

  final def leftShift(n: Int): ByteVector =
    BitVector(this).leftShift(n).toByteVector

  final def length = size

  final def lift(n0: Int): Option[Byte] = {
    if (n0 >= 0 && n0 < size) Some(apply(n0))
    else None
  }

  final def map(f: Byte => Byte): ByteVector =
    ByteVector.view((i: Int) => f(apply(i)), size)

  final def mapI(f: Byte => Int): ByteVector =
    map(f andThen { _.toByte })

  private[scodec] final def mapS(f: F1B): ByteVector =
    ByteVector.view(new At { def apply(i: Int) = f(ByteVector.this(i)) }, size)

  final def nonEmpty: Boolean = !isEmpty

  /** Invokes `compact` on any subtrees whose size is `<= chunkSize`. */
  final def partialCompact(chunkSize: Int): ByteVector = this match {
    case small if small.size <= chunkSize => small.compact
    case Append(l,r) => Append(l.partialCompact(chunkSize), r.partialCompact(chunkSize))
    case _ => this
  }

  final def reverse: ByteVector =
    ByteVector.view(i => apply(size - i - 1), size)

  final def rightShift(n: Int, signExtension: Boolean): ByteVector =
    BitVector(this).rightShift(n, signExtension).toByteVector

  def size: Int

  final def splitAt(n: Int): (ByteVector, ByteVector) = (take(n), drop(n))

  final def slice(from: Int, until: Int): ByteVector =
    drop(from).take(until - from)

  final def tail: ByteVector = drop(1)

  final def take(n: Int): ByteVector = {
    val n1 = n min size max 0
    if (n1 == size) this
    else if (n1 == 0) ByteVector.empty
    else {
      def go(accL: List[ByteVector], cur: ByteVector, n1: Int): ByteVector = cur match {
        case Chunk(bs) => accL.foldLeft(Chunk(bs.take(n1)): ByteVector)((r,l) => l ++ r)
        case Append(l,r) => if (n1 > l.size) go(l :: accL, r, n1-l.size)
                            else go(accL, l, n1)
      }
      go(Nil, this, n1)
    }
  }

  final def takeRight(n: Int): ByteVector =
    drop(size - n)

  /**
   * Converts the contents of this vector to a byte array.
   *
   * @group conversions
   */
  final def toArray: Array[Byte] = {
    val buf = new Array[Byte](size)
    var i = 0
    this.foreachS { new F1BU { def apply(b: Byte) = { buf(i) = b; i += 1 } }}
    buf
  }

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
   * @group conversions
   */
  def toBitVector: BitVector = BitVector(this)

  /**
   * Converts the contents of this vector to a `java.nio.ByteBuffer`.
   *
   * The returned buffer is freshly allocated with limit set to the minimum number of bytes needed
   * to represent the contents of this vector, position set to zero, and remaining set to the limit.
   *
   * @group conversions
   */
  final def toByteBuffer: ByteBuffer = ByteBuffer.wrap(toArray)

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
        val idx = group.padTo(8).rightShift(2, false).toByteVector.head
        bldr.append(alphabet.toChar(idx))
      }
      if (paddingBytes > 0) bldr.append(alphabet.pad.toString * paddingBytes)
    }
    bldr.toString
  }

  final def update(idx: Int, b: Byte): ByteVector = {
    checkIndex(idx)
    (take(idx) :+ b) ++ drop(idx+1)
  }

  final def not: ByteVector = mapS { new F1B { def apply(b: Byte) = (~b).toByte } }

  final def or(other: ByteVector): ByteVector =
    zipWithS(other)(new F2B { def apply(b: Byte, b2: Byte) = (b | b2).toByte })

  final def and(other: ByteVector): ByteVector =
    zipWithS(other)(new F2B { def apply(b: Byte, b2: Byte) = (b & b2).toByte })

  final def xor(other: ByteVector): ByteVector =
    zipWithS(other)(new F2B { def apply(b: Byte, b2: Byte) = (b ^ b2).toByte })

  final def zipWith(other: ByteVector)(f: (Byte,Byte) => Byte): ByteVector =
    zipWithS(other)(new F2B { def apply(b: Byte, b2: Byte) = f(b,b2) })

  private[scodec] final def zipWithS(other: ByteVector)(f: F2B): ByteVector = {
    val at = new At { def apply(i: Int) = f(ByteVector.this(i), other(i)) }
    Chunk(View(at, 0, size min other.size))
  }

  final def zipWithI(other: ByteVector)(op: (Byte, Byte) => Int): ByteVector =
    zipWith(other) { case (l, r) => op(l, r).toByte }

  // implementation details, Object methods

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

  override def equals(other: Any) = other match {
    case that: ByteVector => this.size == that.size &&
                             (0 until this.size).forall(i => this(i) == that(i))
    case other => false
  }

  override def toString: String = s"ByteVector($size bytes, 0x${toHex})"

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
      throw new IllegalArgumentException(s"invalid index: $n for size $size")
}

object ByteVector {

  // various specialized function types
  private[scodec] abstract class At { def apply(i: Int): Byte }
  private[scodec] abstract class F1B { def apply(b: Byte): Byte }
  private[scodec] abstract class F1BU { def apply(b: Byte): Unit }
  private[scodec] abstract class F2B { def apply(b: Byte, b2: Byte): Byte }

  private val AtEmpty: At =
    new At { def apply(i: Int) = throw new IllegalArgumentException("empty view") }

  private def AtArray(arr: Array[Byte]): At =
    new At { def apply(i: Int) = arr(i) }

  private def AtByteBuffer(buf: ByteBuffer): At =
    new At { def apply(i: Int) = buf.get(i) }

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

  val empty: ByteVector = Chunk(View(AtEmpty, 0, 0))

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

  /** Create a `ByteVector` from a `Vector[Byte]`. */
  def apply(bytes: Vector[Byte]): ByteVector = view(bytes, bytes.size)

  /**
   * Create a `ByteVector` from an `Array[Byte]`. The given `Array[Byte]` is
   * is copied to ensure the resulting `ByteVector` is immutable.
   * If this is not desired, use `[[scodec.bits.ByteVector.view]]`.
   */
  def apply(bytes: Array[Byte]): ByteVector = {
    val copy: Array[Byte] = bytes.clone()
    view(copy)
  }

  /**
   * Create a `ByteVector` from a `ByteBuffer`. The given `ByteBuffer` is
   * is copied to ensure the resulting `ByteVector` is immutable.
   * If this is not desired, use `[[scodec.bits.ByteVector.view]]`.
   */
  def apply(buffer: ByteBuffer): ByteVector = {
    val arr = Array.ofDim[Byte](buffer.remaining)
    buffer.get(arr)
    apply(arr)
  }

  /** Create a `ByteVector` from a `scala.collection` source of bytes. */
  def apply(bs: GenTraversableOnce[Byte]): ByteVector =
    view(bs.toArray[Byte])

  /**
   * Create a `ByteVector` from an `Array[Byte]`. Unlike `apply`, this
   * does not make a copy of the input array, so callers should take care
   * not to modify the contents of the array passed to this function.
   */
  def view(bytes: Array[Byte]): ByteVector =
    Chunk(View(AtArray(bytes), 0, bytes.length))

  /**
   * Create a `ByteVector` from a `ByteBuffer`. Unlike `apply`, this
   * does not make a copy of the input buffer, so callers should take care
   * not to modify the contents of the buffer passed to this function.
   */
  def view(bytes: ByteBuffer): ByteVector =
    Chunk(View(AtByteBuffer(bytes), 0, bytes.limit))

  /**
   * Create a `ByteVector` from a function from `Int => Byte` and a size.
   */
  def view(at: Int => Byte, size: Int): ByteVector =
    Chunk(View(new At { def apply(i: Int) = at(i) }, 0, size))

  /**
   * Create a `ByteVector` from a function from `Int => Byte` and a size.
   */
  private[scodec] def view(at: At, size: Int): ByteVector =
    Chunk(View(at, 0, size))

  /**
   * Create a `ByteVector` from a function from `Int => Int` and a size,
   * where the `Int` returned by `at` must fit in a `Byte`.
   */
  def viewI(at: Int => Int, size: Int): ByteVector =
    Chunk(View(new At { def apply(i: Int) = at(i).toByte }, 0, size))

  /**
   * Create a `ByteVector` of the given size, where all bytes have the value `b`.
   */
  def fill[A: Integral](size: Int)(b: A): ByteVector = {
    val integral = implicitly[Integral[A]]
    val v = integral.toInt(b).toByte
    view(Array.fill(size)(v))
  }

  /**
   * Create a `ByteVector` of the given size, where all bytes have the value `0`.
   */
  def low(size: Int): ByteVector = fill(size)(0)

  /**
   * Create a `ByteVector` of the given size, where all bytes have the value `0xff`.
   */
  def high(size: Int): ByteVector = fill(size)(0xff)

  /**
   * Constructs a `ByteVector` from a hexadecimal string or returns an error message if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
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
          ByteVector(result).rightShift(4, false)
        } else ByteVector(bldr.result), count)
      )
    } else Left(err)
  }

  /**
   * Constructs a `ByteVector` from a hexadecimal string or returns `None` if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   */
  def fromHex(str: String, alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase): Option[ByteVector] = fromHexDescriptive(str, alphabet).right.toOption

  /**
   * Constructs a `ByteVector` from a hexadecimal string or throws an IllegalArgumentException if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   *
   * @throws IllegalArgumentException if the string is not valid hexadecimal
   */
  def fromValidHex(str: String, alphabet: Bases.HexAlphabet = Bases.Alphabets.HexLowercase): ByteVector =
    fromHexDescriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)


  /**
   * Constructs a `ByteVector` from a binary string or returns an error message if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
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
        ByteVector(bldr.result).rightShift(8 - bits, false)
      } else {
        ByteVector(bldr.result)
      }, count))
    } else Left(err)
  }

  /**
   * Constructs a `ByteVector` from a binary string or returns `None` if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   */
  def fromBin(str: String, alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary): Option[ByteVector] = fromBinDescriptive(str, alphabet).right.toOption

  /**
   * Constructs a `ByteVector` from a binary string or throws an IllegalArgumentException if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   *
   * @throws IllegalArgumentException if the string is not valid binary
   */
  def fromValidBin(str: String, alphabet: Bases.BinaryAlphabet = Bases.Alphabets.Binary): ByteVector =
    fromBinDescriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)

  /**
   * Constructs a `ByteVector` from a base 64 string or returns an error message if the string is not valid base 64.
   *
   * The string may contain whitespace characters.
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
   */
  def fromBase64(str: String, alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64): Option[ByteVector] = fromBase64Descriptive(str, alphabet).right.toOption

  /**
   * Constructs a `ByteVector` from a base 64 string or throws an IllegalArgumentException if the string is not valid base 64.
   *
   * The string may contain whitespace characters.
   *
   * @throws IllegalArgumentException if the string is not valid base 64
   */
  def fromValidBase64(str: String, alphabet: Bases.Base64Alphabet = Bases.Alphabets.Base64): ByteVector =
    fromBase64Descriptive(str, alphabet).fold(msg => throw new IllegalArgumentException(msg), identity)
}
