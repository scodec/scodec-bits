package scodec.bits

/**
 * Bitwise operations on a value of type `Repr`.
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

  /**
   * Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the left `n` bits.
   *
   * @group bitwise
   */
  final def <<(n: Idx): Repr = shiftLeft(n)

  /**
   * Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the left `n` bits.
   *
   * @group bitwise
   */
  def shiftLeft(n: Idx): Repr

  /**
   * Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are sign extended.
   *
   * @group bitwise
   */
  final def >>(n: Idx): Repr = shiftRight(n, true)

  /**
   * Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are low.
   *
   * @group bitwise
   */
  final def >>>(n: Idx): Repr = shiftRight(n, false)

  /**
   * Returns a $bitwiseOperationsReprDescription of the same size with each bit shifted to the right `n` bits.
   *
   * @param signExtension whether the `n` left-msot bits should take on the value of bit 0
   *
   * @group bitwise
   */
  def shiftRight(n: Idx, signExtension: Boolean): Repr

  /**
   * Returns a $bitwiseOperationsReprDescription of the same size with each bit circularly shifted to the left `n` bits.
   *
   * @group bitwise
   */
  def rotateLeft(n: Idx): Repr

  /**
   * Returns a $bitwiseOperationsReprDescription of the same size with each bit circularly shifted to the right `n` bits.
   *
   * @group bitwise
   */
  def rotateRight(n: Idx): Repr

  /**
   * Returns a bitwise complement of this $bitwiseOperationsReprDescription.
   *
   * @group bitwise
   */
  final def unary_~(): Repr = not

  /**
   * Returns a bitwise complement of this $bitwiseOperationsReprDescription.
   *
   * @group bitwise
   */
  def not: Repr

  /**
   * Returns a bitwise AND of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  final def &(other: Repr): Repr = and(other)

  /**
   * Returns a bitwise AND of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  def and(other: Repr): Repr

  /**
   * Returns a bitwise OR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  final def |(other: Repr): Repr = or(other)

  /**
   * Returns a bitwise OR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  def or(other: Repr): Repr

  /**
   * Returns a bitwise XOR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  final def ^(other: Repr): Repr = xor(other)

  /**
   * Returns a bitwise XOR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  def xor(other: Repr): Repr

  /**
   * Returns a bitwise implication of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  def implies(other: Repr): Repr = not.or(other)

  /**
   * Returns a bitwise if-and-only-if of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  def iff(other: Repr): Repr = xor(other).not

  /**
   * Returns a bitwise NAND of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  def nand(other: Repr): Repr = not.and(other)

  /**
   * Returns a bitwise NOR of this $bitwiseOperationsReprDescription with the specified $bitwiseOperationsReprDescription.
   *
   * $minsize
   *
   * @group bitwise
   */
  def nor(other: Repr): Repr = not.or(other)
}
