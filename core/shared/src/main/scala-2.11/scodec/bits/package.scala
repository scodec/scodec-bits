package scodec

import scala.language.experimental.macros

/**
  * Provides immutable data types for working with bits and bytes.
  *
  * @see [[BitVector]] and [[ByteVector]]
  */
package object bits {

  implicit class EitherOps[A,B](val e: Either[A,B]) extends AnyVal {
    def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = e.right.flatMap(f)
    def map[B1](f: B => B1): Either[A, B1] = e.right.map(f)

    def toOption: Option[B] = e match {
      case Right(b) => Some(b)
      case _        => None
    }
  }

  private[bits] type IterableOnce[+A] = collection.GenTraversableOnce[A]

  private[bits] implicit class IterableOnceOps[A](private val self: IterableOnce[A]) {
    def iterator: Iterator[A] = self.toIterator
  }

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
}
