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

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}

class ByteVectorJsTest extends BitsSuite {
  private def setDEADBEEF(int8Array: Int8Array): Unit = {
    int8Array(0) = 0xde.toByte
    int8Array(1) = 0xad.toByte
    int8Array(2) = 0xbe.toByte
    int8Array(3) = 0xef.toByte
  }

  test("view(Int8Array)") {
    val int8Array = new Int8Array(4)
    setDEADBEEF(int8Array)
    val byteVector: ByteVector = ByteVector.view(int8Array)
    assert(byteVector === hex"deadbeef")
    int8Array(3) = 0xee.toByte
    assert(byteVector === hex"deadbeee")
  }

  test("view(ArrayBuffer)") {
    val arrayBuffer = new ArrayBuffer(4)
    val int8Array = new Int8Array(arrayBuffer)
    setDEADBEEF(int8Array)
    val byteVector: ByteVector = ByteVector.view(arrayBuffer)
    assert(byteVector === hex"deadbeef")
    int8Array(3) = 0xee.toByte
    assert(byteVector === hex"deadbeee")
  }

  test("fromTypedArray") {
    val int8Array = new Int8Array(4)
    setDEADBEEF(int8Array)
    val byteVector: ByteVector = ByteVector.fromTypedArray(int8Array)
    assert(byteVector === hex"deadbeef")
    int8Array(3) = 0xee.toByte
    assert(byteVector === hex"deadbeef")
  }

  test("fromArrayBuffer") {
    val arrayBuffer = new ArrayBuffer(4)
    val int8Array = new Int8Array(arrayBuffer)
    setDEADBEEF(int8Array)
    val byteVector: ByteVector = ByteVector.fromArrayBuffer(arrayBuffer)
    assert(byteVector === hex"deadbeef")
    int8Array(3) = 0xee.toByte
    assert(byteVector === hex"deadbeef")
  }

  test("copyToTypedArray") {
    val int8Array = new Int8Array(6)
    int8Array(0) = 0xaa.toByte
    int8Array(5) = 0xff.toByte
    hex"bbccdeadbeefee".copyToTypedArray(int8Array, 1, 2, 4)
    assert(ByteVector.view(int8Array) === hex"aadeadbeefff")
  }

  test("toTypedArray") {
    assert(ByteVector.view(hex"deadbeef".toTypedArray) === hex"deadbeef")
  }
}
