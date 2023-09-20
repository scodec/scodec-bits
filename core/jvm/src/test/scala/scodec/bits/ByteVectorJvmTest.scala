/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec.bits

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import Arbitrary.arbitrary
import Arbitraries._

import java.util.concurrent.Callable

class ByteVectorJvmTest extends BitsSuite {

  var pool: java.util.concurrent.ExecutorService = null

  override def beforeAll() = {
    super.beforeAll()
    pool = java.util.concurrent.Executors.newFixedThreadPool(4)
  }

  override def afterAll() = {
    pool.shutdownNow()
    super.afterAll()
  }

  property("buffer concurrency") {
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
  }

  property("digest") {
    forAll { (x: ByteVector) =>
      val sha256 = java.security.MessageDigest.getInstance("SHA-256")
      assert(x.sha256 == ByteVector.view(sha256.digest(x.toArray)))
    }
  }

  property("gzip (1)") {
    forAll((x: ByteVector) => assert(x.deflate().inflate() == Right(x)))
  }

  property("gzip (2)") {
    val deflatableByteVectors = for {
      b <- arbitrary[Byte]
      sz <- Gen.chooseNum(1L, 8192L)
    } yield ByteVector.fill(sz)(b)
    forAll(deflatableByteVectors) { (x: ByteVector) =>
      if (x.size > 11) assert(x.deflate().size < x.size)
    }
  }

  property("serialization") {
    forAll((x: ByteVector) => serializationShouldRoundtrip(x))
  }
}
