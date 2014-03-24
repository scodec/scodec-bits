package scodec.bits

/** Enumeration of byte ordering. */
sealed trait ByteOrdering

/** Companion for [[ByteOrdering]]. */
object ByteOrdering {
  /** Byte ordering where the most significant byte is at the smallest address. */
  case object BigEndian extends ByteOrdering

  /** Byte ordering where the least significant byte is at the smallest address. */
  case object LittleEndian extends ByteOrdering
}
