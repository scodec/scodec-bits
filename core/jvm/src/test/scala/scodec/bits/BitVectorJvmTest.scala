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

import java.security.MessageDigest
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import Arbitraries._

class BitVectorJvmTest extends BitsSuite {
  implicit val arbitraryBitVector: Arbitrary[BitVector] = Arbitrary {
    Gen.oneOf(flatBytes, balancedTrees, splitVectors, concatSplitVectors, bitStreams)
  }

  property("sizeGreater/LessThan concurrent") {
    forAll { (x: BitVector) =>
      val ok = new java.util.concurrent.atomic.AtomicBoolean(true)
      def t =
        new Thread {
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
      assert(ok.get == true)
    }
  }

  property("digest") {
    forAll { (x: BitVector) =>
      val sha256 = MessageDigest.getInstance("SHA-256")
      assert(x.digest("SHA-256") == BitVector(ByteVector(sha256.digest(x.toByteArray))))
    }
  }

  property("serialization") {
    forAll((x: BitVector) => serializationShouldRoundtrip(x))
  }

}
