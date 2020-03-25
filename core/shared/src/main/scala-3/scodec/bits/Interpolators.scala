package scodec.bits

import scala.quoted._
import scala.quoted.matching._

/**
  * Provides the `hex` string interpolator, which returns `ByteVector` instances from hexadecimal strings.
  * 
  * @example {{{
  * scala> val b = hex"deadbeef"
  * val b: scodec.bits.ByteVector = ByteVector(4 bytes, 0xdeadbeef)
  * }}}
  */
inline def (ctx: StringContext).hex (inline args: ByteVector*): ByteVector =
  ${hexInterpolator('ctx, 'args)}

private def hexInterpolator(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[ByteVector]])(given qctx: QuoteContext): Expr[ByteVector] = {
  (strCtxExpr, argsExpr) match {
    case ('{ StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args)) =>
      val partValues: Seq[String] = parts.map { case p @ Const(part) =>
        if (ByteVector.fromHex(part).isEmpty)
          qctx.error("hexadecimal string literal may only contain characters [0-9a-fA-f]", p)
        part
      }
      if (partValues.size == 1)
        '{ByteVector.fromValidHex(${Expr(partValues.head)})}
      else {
        val init: Expr[StringBuilder] = '{ new StringBuilder().append(${Expr(partValues.head)}) }
        val bldr: Expr[StringBuilder] = args.zip(partValues.tail).foldLeft(init) { case (sb, (arg, part)) =>
          '{$sb.append($arg.toHex).append(${Expr(part)})}
        }
        '{ByteVector.fromValidHex($bldr.toString)}
      }
  }
}

/**
  * Provides the `bin` string interpolator, which returns `BitVector` instances from binary strings.
  *
  * @example {{{
  * scala> val b = bin"1010101010"
  * val b: scodec.bits.BitVector = BitVector(10 bits, 0xaa8)
  * }}}
  */
inline def (ctx: StringContext).bin (inline args: BitVector*): BitVector =
  ${binInterpolator('ctx, 'args)}

private def binInterpolator(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[BitVector]])(given qctx: QuoteContext): Expr[BitVector] = {
  (strCtxExpr, argsExpr) match {
    case ('{ StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args)) =>
      val partValues: Seq[String] = parts.map { case p @ Const(part) =>
        if (BitVector.fromBin(part).isEmpty)
          qctx.error("binary string literal may only contain characters [0, 1]", p)
        part
      }
      if (partValues.size == 1)
        '{BitVector.fromValidBin(${Expr(partValues.head)})}
      else {
        val init: Expr[StringBuilder] = '{ new StringBuilder().append(${Expr(partValues.head)}) }
        val bldr: Expr[StringBuilder] = args.zip(partValues.tail).foldLeft(init) { case (sb, (arg, part)) =>
          '{$sb.append($arg.toBin).append(${Expr(part)})}
        }
        '{BitVector.fromValidBin($bldr.toString)}
      }
  }
}
