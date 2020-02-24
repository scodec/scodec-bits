package scodec.bits

import hedgehog.{Gen, Range}
import Generators._

class ByteVectorJvmTest extends BitsSuite {

  property("toBase64") {
    genByteVector.forAll.map { b =>
      val guavaB64 = com.google.common.io.BaseEncoding.base64
      assertEquals(ByteVector.view(guavaB64.decode(b.toBase64)), b)
    }
  }

  property("fromBase64") {
    genByteVector.forAll.map { b =>
      val guavaB64 = com.google.common.io.BaseEncoding.base64
      assertEquals(ByteVector.fromValidBase64(guavaB64.encode(b.toArray)), b)
    }
  }

  test("fromBase64 - digit count non-divisble by 4") {
    assert(
      ByteVector.fromBase64Descriptive("A") == Left(
        "Final base 64 quantum had only 1 digit - must have at least 2 digits"
      )
    )
    assert(ByteVector.fromBase64Descriptive("AB") == Right(hex"00"))
    assert(ByteVector.fromBase64Descriptive("ABC") == Right(hex"0010"))
    assert(ByteVector.fromBase64Descriptive("ABCD") == Right(hex"001083"))
    assert(
      ByteVector.fromBase64Descriptive("ABCDA") == Left(
        "Final base 64 quantum had only 1 digit - must have at least 2 digits"
      )
    )
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

  property("buffer concurrency") {
    import java.util.concurrent.Callable
    val pool = java.util.concurrent.Executors.newFixedThreadPool(4)
    // Concurrently append b1.buffer ++ b2 and b1.buffer ++ b3
    // making sure this gives same results as unbuffered appends
    for {
      b1 <- genByteVector.forAll
      b2 <- genByteVector.forAll
      b3 <- genByteVector.forAll
      n <- Gen.int(Range.linear(0, Int.MaxValue)).forAll
    } yield {
      val b1b = b1.bufferBy((n % 50).max(0) + 1)
      val b1b2 = new Callable[ByteVector] { def call = b1b ++ b2 }
      val b1b3 = new Callable[ByteVector] { def call = b1b ++ b3 }
      val rb1b2 = pool.submit(b1b2)
      val rb1b3 = pool.submit(b1b3)
      assertEquals(rb1b2.get, (b1 ++ b2))
      assertEquals(rb1b3.get, (b1 ++ b3))
    }
    // TODO
    // pool.shutdownNow
  }

  property("digest") {
    genByteVector.forAll.map { x =>
      val sha256 = java.security.MessageDigest.getInstance("SHA-256")
      assertEquals(x.digest("SHA-256"), ByteVector.view(sha256.digest(x.toArray)))
    }
  }

  property("gzip") {
    genByteVector.forAll.map { x =>
      assert(x.deflate().inflate() == Right(x))
    }
  }

  property("gzip (2)") {
    val genDeflatableByteVectors = for {
      b <- genByte
      sz <- Gen.long(Range.linear(1L, 8192L))
    } yield ByteVector.fill(sz)(b)

    genDeflatableByteVectors.forAll.map { x =>
      if (x.size > 11) assert(x.deflate().size < x.size)
    }
  }

  property("serialization") {
    genByteVector.forAll.map { x => serializationShouldRoundtrip(x) }
  }
}
