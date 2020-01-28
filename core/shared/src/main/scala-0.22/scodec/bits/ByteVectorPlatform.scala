package scodec.bits

import scala.quoted._
import scala.quoted.matching._
import scala.util.FromDigits

// TODO: as of 0.22, making this private[bits] results in errors like the following at call sites:
// scala> val b: ByteVector = 0x00112233_44556677_8899aabb_ccddeeff_fedba098
// trait ByteVectorPlatform cannot be accessed as a member of scodec.bits from module class rs$line$2$.
trait ByteVectorPlatform { self: ByteVector.type =>

  class FromDigits extends FromDigits.WithRadix[ByteVector] {
    def fromDigits(digits: String, radix: Int): ByteVector =
      digitsToByteVector(digits, radix)
  }

  given FromDigits {
    override inline def fromDigits(digits: String): ByteVector =
      ${digitsToByteVectorMacro('digits, Expr(10))}
    override inline def fromDigits(digits: String, radix: Int): ByteVector =
      ${digitsToByteVectorMacro('digits, 'radix)}
  }
}

private[bits] def digitsToByteVector(digits: String, radix: Int): ByteVector =
  if (radix == 16) ByteVector.fromValidHex(digits.tail)
  else throw FromDigits.MalformedNumber(s"unsupported radix $radix")


def digitsToByteVectorMacro(digits: Expr[String], radix: Expr[Int])(given qctx: QuoteContext): Expr[ByteVector] =
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