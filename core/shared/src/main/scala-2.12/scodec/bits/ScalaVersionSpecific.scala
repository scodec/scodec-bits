package scodec.bits

private[bits] trait ScalaVersionSpecific {

  type IterableOnce[+A] = collection.GenTraversableOnce[A]

  implicit class IterableOnceOps[A](private val self: IterableOnce[A]) {
    def iterator: Iterator[A] = self.toIterator
  }
}
