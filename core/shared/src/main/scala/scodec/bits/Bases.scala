package scodec.bits

/** Provides types related to base conversion -- e.g., binary, hexadecimal, and base 64. */
object Bases {

  /**
   * Partial mapping between characters and indices used in base conversions.
   */
  trait Alphabet {
    /**
     * Converts the specified index to a character.
     * @throws IndexOutOfBoundsException if the specified byte is not supported by this alphabet
     */
    def toChar(index: Int): Char

    /**
     * Converts the specified char to an index.
     * @throws IllegalArgumentException if the specified char is not supported by this alphabet
     */
    def toIndex(c: Char): Int

    /**
     * Indicates whether the specified character should be ignored.
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

  /** An alphabet that supports base 64 conversion. */
  trait Base64Alphabet extends PaddedAlphabet

  /** Predefined alphabets for use in base conversions. */
  object Alphabets {

    /** Binary alphabet that uses `{0, 1}` and allows whitespace and underscores for separation. */
    object Binary extends BinaryAlphabet {
      def toChar(i: Int) = if (i == 0) '0' else '1'
      def toIndex(c: Char) = c match {
        case '0' => 0
        case '1' => 1
        case _ => throw new IllegalArgumentException
      }
      def ignore(c: Char) = c.isWhitespace || c == '_'
    }

    /** Binary alphabet that uses `{t, f}` and allows whitespace and underscores for separation. */
    object Truthy extends BinaryAlphabet {
      def toChar(i: Int) = if (i == 0) 't' else 'f'
      def toIndex(c: Char) = c match {
        case 't' | 'T' => 0
        case 'f' | 'F' => 1
        case _ => throw new IllegalArgumentException
      }
      def ignore(c: Char) = c.isWhitespace || c == '_'
    }

    /** Abstract hex alphabet that supports `{0-9, A-F, a-f}` for looking up an index from a char. */
    private[bits] abstract class LenientHex extends HexAlphabet {
      def toIndex(c: Char) = c match {
        case c if c >= '0' && c <= '9' => c - '0'
        case c if c >= 'a' && c <= 'f' => 10 + (c - 'a')
        case c if c >= 'A' && c <= 'F' => 10 + (c - 'A')
        case c => throw new IllegalArgumentException
      }
      def ignore(c: Char) = c.isWhitespace || c == '_'
    }

    /** Base 16 alphabet that uses `{0-9, a-f}`. Whitespace and underscores are ignored. */
    object HexLowercase extends LenientHex {
      private val Chars = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
      def toChar(i: Int) = Chars(i)
    }

    /** Base 16 alphabet that uses `{0-9, A-F}`. Whitespace and underscores are ignored. */
    object HexUppercase extends LenientHex {
      private val Chars = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
      def toChar(i: Int) = Chars(i)
    }

    /** Base 58 alphabet as defined by [[https://en.bitcoin.it/wiki/Base58Check_encoding#Base58_symbol_chart]]. IPFS hashes uses the same order */
    object Base58 extends Alphabet {
      private val Chars = (('1' to '9') ++ ('A' to 'Z') ++ ('a' to 'z')).filterNot(c => List('O', 'I', 'l').exists(_ == c))
      def toChar(i: Int) = Chars(i)
      def toIndex(c: Char) = c match {
        case c if c >= '1' && c <= '9' => c - '1'
        case c if c >= 'A' && c <= 'H' => c - 'A' + 9
        case c if c >= 'J' && c <= 'N' => c - 'J' + 9 + 8
        case c if c >= 'P' && c <= 'Z' => c - 'P' + 9 + 8 + 5
        case c if c >= 'a' && c <= 'k' => c - 'a' + 9 + 8 + 5 + 11
        case c if c >= 'm' && c <= 'z' => c - 'm' + 9 + 8 + 5 + 11 + 11
        case c => throw new IllegalArgumentException
      }

      def ignore(c: Char) = c.isWhitespace
    }

    /** Base 64 alphabet as defined by [[http://tools.ietf.org/html/rfc4648#section-4 RF4648 section 4]]. Whitespace is ignored. */
    object Base64 extends Base64Alphabet {
      private val Chars = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') :+ '+' :+ '/').toArray
      val pad = '='
      def toChar(i: Int) = Chars(i)
      def toIndex(c: Char) = c match {
        case c if c >= 'A' && c <= 'Z' => c - 'A'
        case c if c >= 'a' && c <= 'z' => c - 'a' + 26
        case c if c >= '0' && c <= '9' => c - '0' + 26 + 26
        case '+' => 62
        case '/' => 63
        case c => throw new IllegalArgumentException
      }
      def ignore(c: Char) = c.isWhitespace
    }

    /** Base 64 alphabet as defined by [[http://tools.ietf.org/html/rfc4648#section-5 RF4648 section 5]]. Whitespace is ignored. */
    object Base64Url extends Base64Alphabet {
      private val Chars = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') :+ '-' :+ '_').toArray
      val pad = '='
      def toChar(i: Int) = Chars(i)
      def toIndex(c: Char) = c match {
        case c if c >= 'A' && c <= 'Z' => c - 'A'
        case c if c >= 'a' && c <= 'z' => c - 'a' + 26
        case c if c >= '0' && c <= '9' => c - '0' + 26 + 26
        case '-' => 62
        case '_' => 63
        case c => throw new IllegalArgumentException
      }
      def ignore(c: Char) = c.isWhitespace
    }

  }
}
