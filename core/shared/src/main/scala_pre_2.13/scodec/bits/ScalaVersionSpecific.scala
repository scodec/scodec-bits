package scodec.bits

private[bits] object ScalaVersionSpecific {

  type IterableOnce[+A] = collection.GenTraversableOnce[A]
}