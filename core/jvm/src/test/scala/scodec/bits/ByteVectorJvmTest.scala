package scodec.bits

import org.scalacheck.{ Arbitrary, Gen }
import Arbitrary.arbitrary
import org.scalatest.Matchers._
import Arbitraries._

class ByteVectorJvmTest extends BitsSuite {

  test("toBase64") {
    forAll { (b: ByteVector) =>
      val guavaB64 = com.google.common.io.BaseEncoding.base64
      ByteVector.view(guavaB64.decode(b.toBase64)) shouldBe b
    }
  }

  test("fromBase64") {
    forAll { (b: ByteVector) =>
      val guavaB64 = com.google.common.io.BaseEncoding.base64
      ByteVector.fromValidBase64(guavaB64.encode(b.toArray)) shouldBe b
    }
  }

  test("buffer concurrency") {
    import java.util.concurrent.Callable
    val pool = java.util.concurrent.Executors.newFixedThreadPool(4)

    // Concurrently append b1.buffer ++ b2 and b1.buffer ++ b3
    // making sure this gives same results as unbuffered appends
    forAll { (b1: ByteVector, b2: ByteVector, b3: ByteVector, n: Int) =>
      val b1b = b1.bufferBy((n % 50).max(0) + 1)
      val b1b2 = new Callable[ByteVector] { def call = b1b ++ b2 }
      val b1b3 = new Callable[ByteVector] { def call = b1b ++ b3 }
      val rb1b2 = pool.submit(b1b2)
      val rb1b3 = pool.submit(b1b3)
      rb1b2.get shouldBe (b1 ++ b2)
      rb1b3.get shouldBe (b1 ++ b3)
    }
    pool.shutdown
  }

  test("digest") {
    forAll { (x: ByteVector) =>
      val sha256 = java.security.MessageDigest.getInstance("SHA-256")
      x.digest("SHA-256") shouldBe ByteVector.view(sha256.digest(x.toArray))
    }
  }

  test("gzip") {
    forAll { (x: ByteVector) =>
      x.deflate().inflate() shouldBe Right(x)
    }

    val deflatableByteVectors = for {
      b <- arbitrary[Byte]
      sz <- Gen.chooseNum(1, 8192)
    } yield ByteVector.fill(sz)(b)
    forAll(deflatableByteVectors) { (x: ByteVector) =>
      if (x.size > 10) x.deflate().size shouldBe < (x.size)
    }
  }

  test("serialization") {
    forAll { (x: ByteVector) => serializationShouldRoundtrip(x) }
  }
}
