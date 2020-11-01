/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec.bits

/** Macros that support binary and hexadecimal literals. */
object LiteralSyntaxMacros {

  @deprecated("Provides compile time compatibility between 2.10 and 2.11", "1.0.6")
  object blackbox { type Context = scala.reflect.macros.Context }

  def binStringInterpolator(c: blackbox.Context)(args: c.Expr[BitVector]*): c.Expr[BitVector] = {
    import c.universe._

    val Apply(_, List(Apply(_, parts))) = c.prefix.tree
    val partLiterals: List[String] = parts.map { case Literal(Constant(part: String)) =>
      if (BitVector.fromBin(part).isEmpty)
        c.error(c.enclosingPosition, "binary string literal may only contain characters [0, 1]")
      part
    }

    val headPart = c.Expr[String](Literal(Constant(partLiterals.head)))
    val initialStringBuilder = reify(new StringBuilder().append(headPart.splice))
    val stringBuilder =
      args.zip(partLiterals.tail).foldLeft(initialStringBuilder) { case (sb, (arg, part)) =>
        val partExpr = c.Expr[String](Literal(Constant(part)))
        reify(sb.splice.append(arg.splice.toBin).append(partExpr.splice))
      }

    reify(BitVector.fromValidBin(stringBuilder.splice.toString))
  }

  def hexStringInterpolator(c: blackbox.Context)(args: c.Expr[ByteVector]*): c.Expr[ByteVector] = {
    import c.universe._

    val Apply(_, List(Apply(_, parts))) = c.prefix.tree
    val partLiterals: List[String] = parts.map { case Literal(Constant(part: String)) =>
      if (ByteVector.fromHex(part).isEmpty)
        c.error(
          c.enclosingPosition,
          "hexadecimal string literal may only contain characters [0-9a-fA-f]"
        )
      part
    }

    val headPart = c.Expr[String](Literal(Constant(partLiterals.head)))
    val initialStringBuilder = reify(new StringBuilder().append(headPart.splice))
    val stringBuilder =
      args.zip(partLiterals.tail).foldLeft(initialStringBuilder) { case (sb, (arg, part)) =>
        val partExpr = c.Expr[String](Literal(Constant(part)))
        reify(sb.splice.append(arg.splice.toHex).append(partExpr.splice))
      }

    reify(ByteVector.fromValidHex(stringBuilder.splice.toString))
  }

}
