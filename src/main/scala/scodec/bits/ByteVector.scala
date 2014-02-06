package scodec.bits

import scala.collection.IndexedSeqOptimized

import java.nio.ByteBuffer

/** Immutable vector of bytes. */
trait ByteVector extends IndexedSeqOptimized[Byte, ByteVector] with BitwiseOperations[ByteVector] {

  def lift(idx: Int): Option[Byte]

  def updated(idx: Int, b: Byte): ByteVector

  def +:(byte: Byte): ByteVector

  def :+(byte: Byte): ByteVector

  def ++(other: ByteVector): ByteVector

  def map(f: Byte => Byte): ByteVector

  def mapI(f: Byte => Int): ByteVector =
    map(f andThen { _.toByte })

  def zipWith(other: ByteVector)(op: (Byte, Byte) => Byte): ByteVector

  def zipWithI(other: ByteVector)(op: (Byte, Byte) => Int): ByteVector =
    zipWith(other) { case (l, r) => op(l, r).toByte }

  def toArray: Array[Byte]

  def toByteBuffer: ByteBuffer = ByteBuffer.wrap(toArray)

  def toBitVector: BitVector = BitVector(this)

  /** Converts the contents of this byte vector to a hexadecimal string of `size * 2` nibbles.  */
  def toHex: String = {
    import ByteVector.HexChars
    val bldr = new StringBuilder
    foreach { b =>
      bldr.append(HexChars(b >> 4 & 0x0f)).append(HexChars(b & 0x0f))
    }
    bldr.toString
  }

  /** Converts the contents of this byte vector to a binary string of `size * 8` digits.  */
  def toBin: String = {
    val bldr = new StringBuilder
    foreach { b =>
      var n = 7
      while (n >= 0) {
        bldr.append(if ((0x01 & (b >> n)) != 0) "1" else "0")
        n -= 1
      }
    }
    bldr.toString
  }

  def leftShift(n: Int): ByteVector =
    BitVector(this).leftShift(n).toByteVector

  def rightShift(n: Int, signExtension: Boolean): ByteVector =
    BitVector(this).rightShift(n, signExtension).toByteVector

  def not: ByteVector = mapI { ~_ }

  def and(other: ByteVector): ByteVector =
    zipWithI(other)(_ & _)

  def or(other: ByteVector): ByteVector =
    zipWithI(other)(_ | _)

  def xor(other: ByteVector): ByteVector =
    zipWithI(other)(_ ^ _)
}

/** Companion for [[ByteVector]]. */
object ByteVector {

  private val HexChars: Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  val empty: ByteVector = StandardByteVector(Vector.empty)

  def apply[A: Integral](bytes: A*): ByteVector = {
    val integral = implicitly[Integral[A]]
    StandardByteVector(bytes.map { i => integral.toInt(i).toByte }.toVector)
  }

  def apply(bytes: Vector[Byte]): ByteVector = StandardByteVector(bytes)

  def apply(bytes: Array[Byte]): ByteVector = StandardByteVector(bytes.toVector)

  def apply(buffer: ByteBuffer): ByteVector = {
    val arr = Array.ofDim[Byte](buffer.remaining)
    buffer.get(arr)
    apply(arr)
  }

  /**
   * Create a `ByteVector` from an `Array[Byte]`. Unlike `apply`, this
   * does not make a copy of the input array, so callers should take care
   * not to modify the contents of the array passed to this function.
   */
  def view(bytes: Array[Byte]): ByteVector =
    new SliceByteVector(ind => bytes(ind), 0, bytes.size)

  /**
   * Create a `ByteVector` from a `ByteBuffer`. Unlike `apply`, this
   * does not make a copy of the input buffer, so callers should take care
   * not to modify the contents of the buffer passed to this function.
   */
  def view(bytes: ByteBuffer): ByteVector =
    new SliceByteVector(ind => bytes.get(ind), 0, bytes.limit)

  /**
   * Create a `ByteVector` from a function from `Int => Byte` and a size.
   */
  def view(at: Int => Byte, size: Int): ByteVector =
    new SliceByteVector(at, 0, size)

  def fill[A: Integral](size: Int)(b: A): ByteVector = {
    val integral = implicitly[Integral[A]]
    StandardByteVector(Vector.fill[Byte](size)(integral.toInt(b).toByte))
  }

  def low(size: Int): ByteVector = fill(size)(0)
  def high(size: Int): ByteVector = fill(size)(0xff)

  /**
   * Constructs a `ByteVector` from a hexadecimal string or returns an error message if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   */
  def fromHexDescriptive(str: String): Either[String, ByteVector] = {
    val prefixed = (str startsWith "0x") || (str startsWith "0X")
    val withoutPrefix = if (prefixed) str.substring(2) else str
    var idx = 0
    var hi: String = null
    var err: String = null
    var bldr = Vector.newBuilder[Byte]
    while (idx < withoutPrefix.length && (err eq null)) {
      withoutPrefix(idx) match {
        case c if (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') =>
          if (hi eq null)
            hi = c.toString
          else {
            bldr += java.lang.Integer.valueOf(hi + c, 16).toByte
            hi = null
          }
        case c if c.isWhitespace || c == '_' =>
          // ignore
        case other =>
          err = s"Invalid hexadecimal character '$other' at index ${idx + (if (prefixed) 2 else 0)}"
      }
      idx += 1
    }
    if (err eq null) {
      Right(
        if (hi eq null) {
          ByteVector(bldr.result)
        } else {
          val lastNibble = java.lang.Integer.valueOf(hi + '0', 16).toByte
          bldr += lastNibble
          ByteVector(bldr.result).rightShift(4, false)
        }
      )
    } else Left(err)
  }

  /**
   * Constructs a `ByteVector` from a hexadecimal string or returns `None` if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   */
  def fromHex(str: String): Option[ByteVector] = fromHexDescriptive(str).right.toOption

  /**
   * Constructs a `ByteVector` from a hexadecimal string or throws an IllegalArgumentException if the string is not valid hexadecimal.
   *
   * The string may start with a `0x` and it may contain whitespace or underscore characters.
   *
   * @throws IllegalArgumentException if the string is not valid hexadecimal
   */
  def fromValidHex(str: String): ByteVector =
    fromHexDescriptive(str).fold(msg => throw new IllegalArgumentException(msg), identity)

  /**
   * Constructs a `ByteVector` from a binary string or returns an error message if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   */
  def fromBinDescriptive(str: String): Either[String, ByteVector] = {
    val prefixed = (str startsWith "0b") || (str startsWith "0B")
    val withoutPrefix = if (prefixed) str.substring(2) else str
    var idx, byte, bits = 0
    var err: String = null
    var bldr = Vector.newBuilder[Byte]
    while (idx < withoutPrefix.length && (err eq null)) {
      withoutPrefix(idx) match {
        case '0' =>
          byte <<= 1
          bits += 1
        case '1' =>
          byte = ((byte << 1) | 1).toByte
          bits += 1
        case c if c.isWhitespace || c == '_' =>
          // ignore
        case other =>
          err = s"Invalid binary character '$other' at index ${idx + (if (prefixed) 2 else 0)}"
      }
      if (bits == 8) {
        bldr += byte.toByte
        byte = 0
        bits = 0
      }
      idx += 1
    }
    if (err eq null) {
      Right(if (bits > 0) {
        bldr += (byte << (8 - bits)).toByte
        ByteVector(bldr.result).rightShift(bits, false)
      } else {
        ByteVector(bldr.result)
      })
    } else Left(err)
  }

  /**
   * Constructs a `ByteVector` from a binary string or returns `None` if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   */
  def fromBin(str: String): Option[ByteVector] = fromBinDescriptive(str).right.toOption

  /**
   * Constructs a `ByteVector` from a binary string or throws an IllegalArgumentException if the string is not valid binary.
   *
   * The string may start with a `0b` and it may contain whitespace or underscore characters.
   *
   * @throws IllegalArgumentException if the string is not valid hexadecimal
   */
  def fromValidBin(str: String): ByteVector =
    fromBinDescriptive(str).fold(msg => throw new IllegalArgumentException(msg), identity)
}
