package scodec.bits

import scala.quoted._
import scala.quoted.matching._
import scala.util.FromDigits

private[bits] trait ByteVectorPlatform { self: ByteVector.type =>
  given as FromDigits.WithRadix[ByteVector] = ByteVectorFromDigits.Instance
}

private[bits] object ByteVectorFromDigits {

  class Base extends FromDigits.WithRadix[ByteVector] {
    def fromDigits(digits: String, radix: Int): ByteVector =
      digitsToByteVector(digits, radix)
  }

  private def digitsToByteVector(digits: String, radix: Int): ByteVector =
    if (radix == 16) ByteVector.fromValidHex(digits.tail)
    else throw FromDigits.MalformedNumber(s"unsupported radix $radix")

  object Instance extends Base {
    override inline def fromDigits(digits: String): ByteVector =
      ${digitsToByteVectorMacro('digits, Expr(10))}
    override inline def fromDigits(digits: String, radix: Int): ByteVector =
      ${digitsToByteVectorMacro('digits, 'radix)}
  }

  private def digitsToByteVectorMacro(digits: Expr[String], radix: Expr[Int])(using qctx: QuoteContext): Expr[ByteVector] =
    (digits, radix) match {
      case (Const(ds), Const(r)) =>
        if (r == 16) {
          '{ByteVector.fromValidHex($digits.tail)}
        } else {
          qctx.error(s"unsupported radix $r", radix)
          '{ByteVector.empty}
        }
      case other =>
        '{digitsToByteVector($digits, $radix)}
    }
}
