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

/** Provides types related to base conversion -- e.g., binary, hexadecimal, and base 64. */
object Bases {

  /** Partial mapping between characters and indices used in base conversions.
    */
  trait Alphabet {

    /** Converts the specified index to a character.
      * @throws IndexOutOfBoundsException if the specified byte is not supported by this alphabet
      */
    def toChar(index: Int): Char

    /** Converts the specified char to an index.
      * @throws IllegalArgumentException if the specified char is not supported by this alphabet
      */
    def toIndex(c: Char): Int

    /** Indicates whether the specified character should be ignored.
      */
    def ignore(c: Char): Boolean
  }

  /** An alphabet that supports padding with a pad character. */
  trait PaddedAlphabet extends Alphabet {

    /** Padding character. */
    val pad: Char
  }

  /** An alphabet that supports binary conversion. */
  trait BinaryAlphabet extends Alphabet

  /** An alphabet that supports hexadecimal conversion. */
  trait HexAlphabet extends Alphabet

  /** An alphabet that supports base 32 conversion. */
  trait Base32Alphabet extends PaddedAlphabet

  /** An alphabet that supports base 64 conversion. */
  trait Base64Alphabet extends PaddedAlphabet

  /** Predefined alphabets for use in base conversions. */
  object Alphabets {

    /** Binary alphabet that uses `{0, 1}` and allows whitespace and underscores for separation. */
    object Binary extends BinaryAlphabet {
      def toChar(i: Int) = if (i == 0) '0' else '1'
      def toIndex(c: Char) =
        c match {
          case '0' => 0
          case '1' => 1
          case _   => throw new IllegalArgumentException
        }
      def ignore(c: Char) = c.isWhitespace || c == '_'
    }

    /** Binary alphabet that uses `{t, f}` and allows whitespace and underscores for separation. */
    object Truthy extends BinaryAlphabet {
      def toChar(i: Int) = if (i == 0) 't' else 'f'
      def toIndex(c: Char) =
        c match {
          case 't' | 'T' => 0
          case 'f' | 'F' => 1
          case _         => throw new IllegalArgumentException
        }
      def ignore(c: Char) = c.isWhitespace || c == '_'
    }

    /** Abstract hex alphabet that supports `{0-9, A-F, a-f}` for looking up an index from a char. */
    private[bits] abstract class LenientHex extends HexAlphabet {
      def toIndex(c: Char) =
        c match {
          case c if c >= '0' && c <= '9' => c - '0'
          case c if c >= 'a' && c <= 'f' => 10 + (c - 'a')
          case c if c >= 'A' && c <= 'F' => 10 + (c - 'A')
          case _                         => throw new IllegalArgumentException
        }
      def ignore(c: Char) = c.isWhitespace || c == '_'
    }

    /** Base 16 alphabet that uses `{0-9, a-f}`. Whitespace and underscores are ignored. */
    object HexLowercase extends LenientHex {
      private val Chars =
        Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
      def toChar(i: Int) = Chars(i)
    }

    /** Base 16 alphabet that uses `{0-9, A-F}`. Whitespace and underscores are ignored. */
    object HexUppercase extends LenientHex {
      private val Chars =
        Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
      def toChar(i: Int) = Chars(i)
    }

    private def charIndicesLookupArray(indicesMap: Map[Char, Int]): (Int, Array[Int]) = {
      val indicesMin: Int = indicesMap.keys.min.toInt
      val indices: Array[Int] = Array.tabulate[Int](indicesMap.keys.max - indicesMin + 1) { i =>
        indicesMap.getOrElse((i + indicesMin).toChar, -1)
      }
      (indicesMin, indices)
    }

    /** Base 32 alphabet, with padding, as defined by [[https://tools.ietf.org/html/rfc4648#section-6 RF4648 section 4]]. Whitespace is ignored. */
    object Base32 extends Base32Alphabet {
      private val Chars: Array[Char] = (('A' to 'Z') ++ ('2' to '7')).toArray
      private val (indicesMin, indices) = charIndicesLookupArray(Chars.zipWithIndex.toMap)
      val pad = '='
      def toChar(i: Int) = Chars(i)
      def toIndex(c: Char) = {
        val lookupIndex = c - indicesMin
        if (lookupIndex >= 0 && lookupIndex < indices.length && indices(lookupIndex) >= 0)
          indices(lookupIndex)
        else throw new IllegalArgumentException
      }
      def ignore(c: Char) = c.isWhitespace
    }

    /** Base 32 Crockford alphabet as defined by [[https://www.crockford.com/base32.html]]. Whitespace and hyphen is ignored. */
    object Base32Crockford extends Base32Alphabet {
      private val Chars: Array[Char] =
        (('0' to '9') ++ ('A' to 'H') ++ ('J' to 'K') ++ ('M' to 'N') ++ ('P' to 'T') ++ ('V' to 'Z')).toArray
      private val (indicesMin, indices) = charIndicesLookupArray {
        val map = (Chars.zipWithIndex ++ Chars.map(_.toLower).zipWithIndex).toMap
        map ++ Map(
          'O' -> map('0'),
          'o' -> map('0'),
          'I' -> map('1'),
          'i' -> map('1'),
          'L' -> map('1'),
          'l' -> map('1')
        )
      }
      val pad = '='
      def toChar(i: Int) = Chars(i)
      def toIndex(c: Char) = {
        val lookupIndex = c - indicesMin
        if (lookupIndex >= 0 && lookupIndex < indices.length && indices(lookupIndex) >= 0)
          indices(lookupIndex)
        else throw new IllegalArgumentException
      }
      def ignore(c: Char) = c == '-' || c.isWhitespace
    }

    /** Base 58 alphabet as defined by [[https://en.bitcoin.it/wiki/Base58Check_encoding#Base58_symbol_chart]]. IPFS hashes uses the same order. */
    object Base58 extends Alphabet {
      private val Chars = (('1' to '9') ++ ('A' to 'Z') ++ ('a' to 'z'))
        .filterNot(c => List('O', 'I', 'l').contains(c))
        .toArray
      def toChar(i: Int) = Chars(i)
      def toIndex(c: Char) =
        c match {
          case c if c >= '1' && c <= '9' => c - '1'
          case c if c >= 'A' && c <= 'H' => c - 'A' + 9
          case c if c >= 'J' && c <= 'N' => c - 'J' + 9 + 8
          case c if c >= 'P' && c <= 'Z' => c - 'P' + 9 + 8 + 5
          case c if c >= 'a' && c <= 'k' => c - 'a' + 9 + 8 + 5 + 11
          case c if c >= 'm' && c <= 'z' => c - 'm' + 9 + 8 + 5 + 11 + 11
          case _                         => throw new IllegalArgumentException
        }

      def ignore(c: Char) = c.isWhitespace
    }

    private object Base64Base {
      private val Chars = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') :+ '+' :+ '/').toArray
    }

    sealed trait Base64Base extends Base64Alphabet {
      override val pad = '='
      override def toChar(i: Int) = Base64Base.Chars(i)
      override def toIndex(c: Char) =
        c match {
          case c if c >= 'A' && c <= 'Z' => c - 'A'
          case c if c >= 'a' && c <= 'z' => c - 'a' + 26
          case c if c >= '0' && c <= '9' => c - '0' + 26 + 26
          case '+'                       => 62
          case '/'                       => 63
          case _                         => throw new IllegalArgumentException
        }
      override def ignore(c: Char) = c.isWhitespace
    }

    /** Base 64 alphabet, with padding, as defined by [[https://tools.ietf.org/html/rfc4648#section-4 RF4648 section 4]]. Whitespace is ignored. */
    object Base64 extends Base64Base with PaddedAlphabet

    /** Base 64 alphabet, without padding, as defined by [[https://tools.ietf.org/html/rfc4648#section-4 RF4648 section 4]]. Whitespace is ignored. */
    object Base64NoPad extends Base64Base {
      override val pad = 0.toChar
    }

    private object Base64UrlBase {
      private val Chars = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') :+ '-' :+ '_').toArray
    }

    /** Base 64 alphabet, with padding, as defined by [[https://tools.ietf.org/html/rfc4648#section-5 RF4648 section 5]]. Whitespace is ignored. */
    sealed trait Base64UrlBase extends Base64Alphabet {
      override val pad = '='
      override def toChar(i: Int) = Base64UrlBase.Chars(i)
      override def toIndex(c: Char) =
        c match {
          case c if c >= 'A' && c <= 'Z' => c - 'A'
          case c if c >= 'a' && c <= 'z' => c - 'a' + 26
          case c if c >= '0' && c <= '9' => c - '0' + 26 + 26
          case '-'                       => 62
          case '_'                       => 63
          case _                         => throw new IllegalArgumentException
        }
      override def ignore(c: Char) = c.isWhitespace
    }

    object Base64Url extends Base64UrlBase

    object Base64UrlNoPad extends Base64UrlBase {
      override val pad = 0.toChar
    }
  }
}
