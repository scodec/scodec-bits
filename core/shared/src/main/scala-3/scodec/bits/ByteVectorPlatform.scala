package scodec.bits

import scala.util.FromDigits

private[bits] trait ByteVectorPlatform { self: ByteVector.type =>
  given as FromDigits.WithRadix[ByteVector] {
    def fromDigits(digits: String, radix: Int): ByteVector = radix match {
      case 16 => ByteVector.fromValidHex(digits)
      case _ => ByteVector.fromValidHex(new java.math.BigInteger(digits, radix).toString(16))
    }
  }
}
