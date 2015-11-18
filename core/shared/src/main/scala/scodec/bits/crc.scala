package scodec.bits

/**
 * Provides support for calculating cyclic redundancy checks.
 *
 * @see http://www.repairfaq.org/filipg/LINK/F_crc_v3.html
 */
object crc {

  /** 32-bit CRC using poly 0x04c11db7, initial 0xffffffff, reflected input/output, and final xor 0xffffffff. */
  lazy val crc32: BitVector => BitVector =
    int32(0x04c11db7, 0xffffffff, true, true, 0xffffffff) andThen { i => BitVector.fromInt(i) }

  /** 32-bit CRC using poly 0x1edc6f41, initial 0xffffffff, reflected input/output, and final xor 0xffffffff. */
  lazy val crc32c: BitVector => BitVector =
    int32(0x1edc6f41, 0xffffffff, true, true, 0xffffffff) andThen { i => BitVector.fromInt(i) }

  /**
   * Constructs a table-based CRC function using the specified polynomial.
   *
   * Each of the input vectors must be the same size.
   *
   * @return function that calculates a `n`-bit CRC where `n = poly.size`
   */
  def apply(poly: BitVector, initial: BitVector, reflectInput: Boolean, reflectOutput: Boolean, finalXor: BitVector): BitVector => BitVector = {
    require(poly.nonEmpty, "empty polynomial")
    require(initial.size == poly.size && poly.size == finalXor.size, "poly, initial, and finalXor must be same length")

    if (poly.size == 32L) {
      int32(poly.toInt(), initial.toInt(), reflectInput, reflectOutput, finalXor.toInt()) andThen { i => BitVector.fromInt(i) }
    } else {
      vectorTable(poly, initial, reflectInput, reflectOutput, finalXor)
    }
  }

  private[bits] def vectorTable(poly: BitVector, initial: BitVector, reflectInput: Boolean, reflectOutput: Boolean, finalXor: BitVector): BitVector => BitVector = {
    val table = Array.ofDim[BitVector](256)
    val zeroed = BitVector.fill(poly.size - 8)(false)
    val m = 8L
    @annotation.tailrec
    def calculateTableIndex(idx: Int): Unit = {
      if (idx < table.size) {
        @annotation.tailrec
        def shift(k: Int, crcreg: BitVector): BitVector = {
          if (k < m) {
            shift(k + 1, {
              val shifted = crcreg << 1
              if (crcreg.head) shifted xor poly else shifted
            })
          } else crcreg
        }
        table(idx) = shift(0, ByteVector(idx).bits ++ zeroed).compact
        calculateTableIndex(idx + 1)
      }
    }
    calculateTableIndex(0)

    def output(crcreg: BitVector): BitVector =
      (if (reflectOutput) crcreg.reverse else crcreg) xor finalXor

    @annotation.tailrec
    def calculate(remaining: BitVector, crcreg: BitVector): BitVector = {
      if (remaining.isEmpty) {
        output(crcreg)
      } else if (remaining sizeLessThan 8) {
        output(goBitwise(poly, if (reflectInput) remaining.reverseBitOrder else remaining, crcreg))
      } else {
        val shifted = crcreg << m
        val inputByte = remaining.take(m)
        val index = crcreg.take(m) xor (if (reflectInput) inputByte.reverse else inputByte)
        val indexAsInt = index.bytes.head.toInt & 0x0ff
        calculate(remaining drop m, shifted xor table(indexAsInt))
      }
    }

    if (poly.size < 8) a => output(goBitwise(poly, if (reflectInput) a.reverseBitOrder else a, initial))
    else a => calculate(a, initial)
  }

  private def goBitwise(poly: BitVector, remaining: BitVector, crcreg: BitVector): BitVector =
    if (remaining.isEmpty) crcreg
    else goBitwise(poly, remaining.tail, {
      val shifted = crcreg << 1
      if (crcreg.head == remaining.head) shifted else shifted xor poly
    })

  /**
   * Constructs a 32-bit, table-based CRC function using the specified polynomial.
   *
   * @return function that calculates a 32-bit CRC
   */
  def int32(poly: Int, initial: Int, reflectInput: Boolean, reflectOutput: Boolean, finalXor: Int): BitVector => Int = {
    val table = Array.ofDim[Int](256)
    val m = 8L
    @annotation.tailrec
    def calculateTableIndex(idx: Int): Unit = {
      if (idx < table.size) {
        @annotation.tailrec
        def shift(k: Int, crcreg: Int): Int = {
          if (k < m) {
            shift(k + 1, {
              val shifted = crcreg << 1
              if ((crcreg & 0x80000000) != 0) shifted ^ poly else shifted
            })
          } else crcreg
        }
        table(idx) = shift(0, idx << 24)
        calculateTableIndex(idx + 1)
      }
    }
    calculateTableIndex(0)

    def output(crcreg: Int): Int =
      (if (reflectOutput) BitVector.fromInt(crcreg).reverse.toInt() else crcreg) ^ finalXor

    @annotation.tailrec
    def calculate(remaining: BitVector, crcreg: Int): Int = {
      if (remaining.isEmpty) {
        output(crcreg)
      } else if (remaining sizeLessThan 8) {
        output(goBitwise(poly, if (reflectInput) remaining.reverseBitOrder else remaining, crcreg))
      } else {
        val shifted = crcreg << m
        val inputByte = remaining.take(m)
        val index = (crcreg >>> 24) ^ (if (reflectInput) inputByte.reverse else inputByte).toInt(signed = false)
        calculate(remaining drop m, shifted ^ table(index))
      }
    }

    a => calculate(a, initial)
  }

  private def goBitwise(poly: Int, remaining: BitVector, crcreg: Int): Int =
    if (remaining.isEmpty) crcreg
    else goBitwise(poly, remaining.tail, {
      val shifted = crcreg << 1
      if (((crcreg & 0x80000000) != 0) == remaining.head) shifted else shifted ^ poly
    })

  /**
   * Calculates a bitwise CRC of the specified value.
   *
   * If calculating a lot of CRCs, prefer the `apply` method, which precomputes a lookup table
   * and uses it in each CRC calculation.
   *
   * @return function that calculates a `n`-bit CRC where `n = poly.size`
   */
  def bitwise(poly: BitVector, initial: BitVector, reflectInput: Boolean, reflectOutput: Boolean, finalXor: BitVector, value: BitVector): BitVector = {
    val reg = goBitwise(poly, if (reflectInput) value.reverseBitOrder else value, initial)
    (if (reflectOutput) reg.reverse else reg) xor finalXor
  }
}
