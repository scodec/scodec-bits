package scodec.bits

import scala.quoted._
import scala.quoted.matching._

def digitsToBitVectorMacro(digits: Expr[String], radix: Expr[Int])(using qctx: QuoteContext): Expr[BitVector] =
  (digits, radix) match {
    case (Const(ds), Const(r)) =>
      if (r == 16) {
        '{ByteVector.fromValidHex($digits.tail).bits}
      } else {
        qctx.error(s"unsupported radix $r", radix)
        '{BitVector.empty}
      }
    case other =>
      '{BitVectorFromDigits.digitsToBitVector($digits, $radix)}
  }

def digitsToByteVectorMacro(digits: Expr[String], radix: Expr[Int])(using qctx: QuoteContext): Expr[ByteVector] =
  (digits, radix) match {
    case (Const(ds), Const(r)) =>
      if (r == 16) {
        '{ByteVector.fromValidHex($digits.tail)}
      } else {
        qctx.error(s"unsupported radix $r", radix)
        '{ByteVector.empty}
      }
    case other =>
      '{ByteVectorFromDigits.digitsToByteVector($digits, $radix)}
  }

