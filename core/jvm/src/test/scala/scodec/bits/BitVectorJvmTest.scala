package scodec.bits

import java.security.MessageDigest
import Generators._

class BitVectorJvmTest extends BitsSuite {

  test("sizeGreater/LessThan concurrent") {
    genBitVector.forAll.map { x =>
      val ok = new java.util.concurrent.atomic.AtomicBoolean(true)
      def t = new Thread {
        override def start =
          (0 until x.size.toInt).foreach { i =>
            ok.compareAndSet(true, x.sizeGreaterThan(i.toLong))
            ()
          }
      }
      val t1 = t
      val t2 = t
      t1.start
      t2.start
      ok.compareAndSet(true, x.sizeLessThan(x.size + 1))
      t1.join
      t2.join
      assert(ok.get)
    }
  }

  test("digest") {
    genBitVector.forAll.map { x =>
      val sha256 = MessageDigest.getInstance("SHA-256")
      assertEquals(x.digest("SHA-256"), BitVector(ByteVector(sha256.digest(x.toByteArray))))
    }
  }

  test("serialization") {
    genBitVector.forAll.map { x =>
      serializationShouldRoundtrip(x)
    }
  }
}
