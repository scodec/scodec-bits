package scodec.bits

import java.security.MessageDigest
import org.scalacheck.{Arbitrary, Gen}
import Arbitraries._
import org.scalatest.matchers.should.Matchers._

class BitVectorJvmTest extends BitsSuite {
  implicit val arbitraryBitVector: Arbitrary[BitVector] = Arbitrary {
    Gen.oneOf(flatBytes, balancedTrees, splitVectors, concatSplitVectors, bitStreams)
  }

  test("sizeGreater/LessThan concurrent") { forAll { (x: BitVector) =>
    val ok = new java.util.concurrent.atomic.AtomicBoolean(true)
    def t = new Thread {
      override def start = {
        (0 until x.size.toInt).foreach { i =>
          ok.compareAndSet(true, x.sizeGreaterThan(i.toLong))
          ()
        }
      }
    }
    val t1 = t
    val t2 = t
    t1.start
    t2.start
    ok.compareAndSet(true, x.sizeLessThan(x.size+1))
    t1.join
    t2.join
    ok.get shouldBe true
  }}

  test("digest") {
    forAll { (x: BitVector) =>
      val sha256 = MessageDigest.getInstance("SHA-256")
      x.digest("SHA-256") shouldBe BitVector(ByteVector(sha256.digest(x.toByteArray)))
    }
  }

  test("serialization") {
    forAll { (x: BitVector) => serializationShouldRoundtrip(x) }
  }

}
