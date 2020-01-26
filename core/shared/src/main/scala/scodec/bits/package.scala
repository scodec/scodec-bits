package scodec

import scala.language.experimental.macros

/**
  * Provides immutable data types for working with bits and bytes.
  *
  * @see [[BitVector]] and [[ByteVector]]
  */
package object bits extends ScalaVersionSpecific {

  /**
    * Provides the `bin` string interpolator, which returns `BitVector` instances from binary strings.
    */
  final implicit class BinStringSyntax(val sc: StringContext) extends AnyVal {

    /**
      * Converts this binary literal string to a `BitVector`. Whitespace characters are ignored.
      *
      * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
      * of type `BitVector`.
      */
    def bin(args: BitVector*): BitVector = macro LiteralSyntaxMacros.binStringInterpolator
  }

  /**
    * Provides the `hex` string interpolator, which returns `ByteVector` instances from hexadecimal strings.
    */
  final implicit class HexStringSyntax(val sc: StringContext) extends AnyVal {

    /**
      * Converts this hexadecimal literal string to a `ByteVector`. Whitespace characters are ignored.
      *
      * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
      * of type `ByteVector`.
      */
    def hex(args: ByteVector*): ByteVector = macro LiteralSyntaxMacros.hexStringInterpolator
  }

  private[bits] implicit class EitherOps[L, R](val self: Either[L, R]) extends AnyVal {
    def map[R2](f: R => R2): Either[L, R2] = self match {
      case Right(r)      => Right(f(r))
      case l: Left[L, R] => l.asInstanceOf[Either[L, R2]]
    }

    def flatMap[R2](f: R => Either[L, R2]): Either[L, R2] = self match {
      case Right(r)      => f(r)
      case l: Left[L, R] => l.asInstanceOf[Either[L, R2]]
    }

    def toOption: Option[R] = self match {
      case Right(r) => Some(r)
      case Left(_)  => None
    }
  }
}
