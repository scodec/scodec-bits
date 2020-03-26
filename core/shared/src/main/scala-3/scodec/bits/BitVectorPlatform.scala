package scodec.bits

import scala.util.FromDigits

private[bits] trait BitVectorPlatform { self: BitVector.type =>
  given as FromDigits.WithRadix[BitVector] {
    def fromDigits(digits: String, radix: Int): BitVector = radix match {
      case 16 => ByteVector.fromValidHex(digits).bits
      case _ => ByteVector.fromValidHex(new java.math.BigInteger(digits, radix).toString(16)).bits
    }
  }
}
