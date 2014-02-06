package scodec

import scala.language.experimental.macros
import scala.language.implicitConversions

/**
 * Provides immutable data types for working with bits and bytes.
 */
package object bits {

  /** Implicit conversion from `ByteVector` to `BitVector`. */
  implicit def byteVectorToBitVector(byteVector: ByteVector): BitVector = byteVector.toBitVector

  /**
   * Provides the `bin` string interpolator, which returns `BitVector` instances from binary strings.
   *
   * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
   * of type `BitVector`.
   */
  final implicit class BinStringSyntax(val sc: StringContext) extends AnyVal {
    def bin(args: BitVector*): BitVector = macro LiteralSyntaxMacros.binStringInterpolator
  }

  /**
   * Provides the `hex` string interpolator, which returns `ByteVector` instances from hexadecimal strings.
   *
   * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
   * of type `ByteVector`.
   */
  final implicit class HexStringSyntax(val sc: StringContext) extends AnyVal {
    def hex(args: ByteVector*): ByteVector = macro LiteralSyntaxMacros.hexStringInterpolator
  }
}
