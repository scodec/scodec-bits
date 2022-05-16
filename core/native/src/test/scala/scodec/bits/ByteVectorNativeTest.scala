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

import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.alloc

class ByteVectorNativeTest extends BitsSuite {
  private def setDEADBEEF(ptr: Ptr[Byte]): Unit = {
    ptr(0) = 0xde.toByte
    ptr(1) = 0xad.toByte
    ptr(2) = 0xbe.toByte
    ptr(3) = 0xef.toByte
  }

  test("view(Ptr[Byte])") {
    Zone { implicit z =>
      val ptr = alloc[Byte](4)
      setDEADBEEF(ptr)
      val byteVector: ByteVector = ByteVector.view(ptr, 4)
      assert(byteVector === hex"deadbeef")
      ptr(3) = 0xee.toByte
      assert(byteVector === hex"deadbeee")
    }
  }

  test("fromBytePtr") {
    Zone { implicit z =>
      val ptr = alloc[Byte](4)
      setDEADBEEF(ptr)
      val byteVector: ByteVector = ByteVector.fromBytePtr(ptr, 4)
      assert(byteVector === hex"deadbeef")
      ptr(3) = 0xee.toByte
      assert(byteVector === hex"deadbeef")
    }
  }

  test("copyToBytePtr") {
    Zone { implicit z =>
      val ptr = alloc[Byte](6)
      ptr(0) = 0xaa.toByte
      ptr(5) = 0xff.toByte
      hex"bbccdeadbeefee".copyToBytePtr(ptr, 1, 2, 4)
      assert(ByteVector.view(ptr, 6) === hex"aadeadbeefff")
    }
  }

  test("toBytePtr") {
    Zone { implicit z =>
      assert(ByteVector.view(hex"deadbeef".toBytePtr, 4) === hex"deadbeef")
    }
  }
}
