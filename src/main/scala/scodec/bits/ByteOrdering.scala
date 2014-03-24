package scodec.bits

import java.nio.ByteOrder

/** Enumeration of byte ordering. */
sealed trait ByteOrdering {
  def toJava: ByteOrder
}

/** Companion for [[ByteOrdering]]. */
object ByteOrdering {

  /**
   * Converts the specified byte order to a [[ByteOrdering]].
   * @throws IllegalArgumentException if the order cannot be converted
   */
  def fromJava(bo: ByteOrder): ByteOrdering =
    if (bo == ByteOrder.BIG_ENDIAN) ByteOrdering.BigEndian
    else if (bo == ByteOrder.LITTLE_ENDIAN) ByteOrdering.LittleEndian
    else throw new IllegalArgumentException("unknown byte order " + bo)

  /** Byte ordering where the most significant byte is at the smallest address. */
  case object BigEndian extends ByteOrdering {
    def toJava = ByteOrder.BIG_ENDIAN
  }

  /** Byte ordering where the least significant byte is at the smallest address. */
  case object LittleEndian extends ByteOrdering {
    def toJava = ByteOrder.LITTLE_ENDIAN
  }
}
