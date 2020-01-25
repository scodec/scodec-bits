package scodec.bits

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import Arbitraries._

class ByteVectorJvmTest extends BitsSuite {

  test("toBase64") {
    forAll { (b: ByteVector) =>
      val guavaB64 = com.google.common.io.BaseEncoding.base64
      assert(ByteVector.view(guavaB64.decode(b.toBase64)) == b)
    }
  }

  test("fromBase64") {
    forAll { (b: ByteVector) =>
      val guavaB64 = com.google.common.io.BaseEncoding.base64
      assert(ByteVector.fromValidBase64(guavaB64.encode(b.toArray)) == b)
    }
  }

  test("fromBase64 - digit count non-divisble by 4") {
    assert(ByteVector.fromBase64Descriptive("A") == Left(
      "Final base 64 quantum had only 1 digit - must have at least 2 digits"
    ))
    assert(ByteVector.fromBase64Descriptive("AB") == Right(hex"00"))
    assert(ByteVector.fromBase64Descriptive("ABC") == Right(hex"0010"))
    assert(ByteVector.fromBase64Descriptive("ABCD") == Right(hex"001083"))
    assert(ByteVector.fromBase64Descriptive("ABCDA") == Left(
      "Final base 64 quantum had only 1 digit - must have at least 2 digits"
    ))
    assert(ByteVector.fromBase64Descriptive("ABCDAB") == Right(hex"00108300"))
  }

  test("fromBase64 - padding") {
    assert(ByteVector.fromBase64Descriptive("AB==") == Right(hex"00"))
    val paddingError = Left(
      "Malformed padding - final quantum may optionally be padded with one or two padding characters such that the quantum is completed"
    )
    assert(ByteVector.fromBase64Descriptive("A=") == paddingError)
    assert(ByteVector.fromBase64Descriptive("A==") == paddingError)
    assert(ByteVector.fromBase64Descriptive("A===") == paddingError)
    assert(ByteVector.fromBase64Descriptive("A====") == paddingError)
    assert(ByteVector.fromBase64Descriptive("AB=") == paddingError)
    assert(ByteVector.fromBase64Descriptive("AB===") == paddingError)
    assert(ByteVector.fromBase64Descriptive("ABC==") == paddingError)
    assert(ByteVector.fromBase64Descriptive("=") == paddingError)
    assert(ByteVector.fromBase64Descriptive("==") == paddingError)
    assert(ByteVector.fromBase64Descriptive("===") == paddingError)
    assert(ByteVector.fromBase64Descriptive("====") == paddingError)
    assert(ByteVector.fromBase64Descriptive("=====") == paddingError)
  }

  test("fromBase64 - empty input string") {
    assert(ByteVector.fromBase64Descriptive("") == Right(ByteVector.empty))
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
      assert(rb1b2.get == (b1 ++ b2))
      assert(rb1b3.get == (b1 ++ b3))
    }
    pool.shutdown
  }

  test("digest") {
    forAll { (x: ByteVector) =>
      val sha256 = java.security.MessageDigest.getInstance("SHA-256")
      assert(x.digest("SHA-256") == ByteVector.view(sha256.digest(x.toArray)))
    }
  }

  test("gzip") {
    forAll { (x: ByteVector) =>
      assert(x.deflate().inflate() == Right(x))
    }

    val deflatableByteVectors = for {
      b <- arbitrary[Byte]
      sz <- Gen.chooseNum(1L, 8192L)
    } yield ByteVector.fill(sz)(b)
    forAll(deflatableByteVectors) { (x: ByteVector) =>
      if (x.size > 11) assert(x.deflate().size < x.size)
    }
  }

  test("serialization") {
    forAll { (x: ByteVector) =>
      serializationShouldRoundtrip(x)
    }
  }
}
