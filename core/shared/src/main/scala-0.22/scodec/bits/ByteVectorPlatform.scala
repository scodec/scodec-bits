package scodec.bits

import scala.quoted._
import scala.quoted.matching._
import scala.util.FromDigits

trait ByteVectorPlatform {

  class FromDigits extends FromDigits.WithRadix[ByteVector] {
    def fromDigits(digits: String, radix: Int): ByteVector =
      if (radix == 16) ByteVector.fromValidHex(digits.tail)
      else throw FromDigits.MalformedNumber("unsupported radix")
  }

  given FromDigits {
    override inline def fromDigits(digits: String): ByteVector =
      ${digitsToByteVector('digits, Expr(10))}
    override inline def fromDigits(digits: String, radix: Int): ByteVector =
      ${digitsToByteVector('digits, 'radix)}
  }
}


def digitsToByteVector(digits: Expr[String], radix: Expr[Int])(given qctx: QuoteContext): Expr[ByteVector] =
  (digits, radix) match {
    case (Const(ds), Const(r)) =>
      if (r == 16) {
        '{ByteVector.fromValidHex($digits.tail)}
      } else {
        qctx.error(s"unsupported radix $r", radix)
        '{ByteVector.empty}
      }
    case other =>
      '{
          if ($radix == 16) ByteVector.fromValidHex($digits.tail)
          else throw FromDigits.MalformedNumber("unsupported radix")
      }
  }