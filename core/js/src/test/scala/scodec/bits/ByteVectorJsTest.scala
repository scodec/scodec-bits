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

import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

class ByteVectorJsTest extends BitsSuite {
  private def setDEADBEEF(uint8Array: Uint8Array): Unit = {
    uint8Array(0) = 0xde.toByte
    uint8Array(1) = 0xad.toByte
    uint8Array(2) = 0xbe.toByte
    uint8Array(3) = 0xef.toByte
  }

  test("view(Uint8Array)") {
    val uint8Array = new Uint8Array(4)
    setDEADBEEF(uint8Array)
    val byteVector: ByteVector = ByteVector.view(uint8Array)
    assert(byteVector === hex"deadbeef")
    uint8Array(3) = 0xee.toByte
    assert(byteVector === hex"deadbeee")
  }

  test("view(ArrayBuffer)") {
    val arrayBuffer = new ArrayBuffer(4)
    val uint8Array = new Uint8Array(arrayBuffer)
    setDEADBEEF(uint8Array)
    val byteVector: ByteVector = ByteVector.view(arrayBuffer)
    assert(byteVector === hex"deadbeef")
    uint8Array(3) = 0xee.toByte
    assert(byteVector === hex"deadbeee")
  }

  test("fromTypedArray") {
    val uint8Array = new Uint8Array(4)
    setDEADBEEF(uint8Array)
    val byteVector: ByteVector = ByteVector.fromUint8Array(uint8Array)
    assert(byteVector === hex"deadbeef")
    uint8Array(3) = 0xee.toByte
    assert(byteVector === hex"deadbeef")
  }

  test("fromArrayBuffer") {
    val arrayBuffer = new ArrayBuffer(4)
    val uint8Array = new Uint8Array(arrayBuffer)
    setDEADBEEF(uint8Array)
    val byteVector: ByteVector = ByteVector.fromJSArrayBuffer(arrayBuffer)
    assert(byteVector === hex"deadbeef")
    uint8Array(3) = 0xee.toByte
    assert(byteVector === hex"deadbeef")
  }

  test("copyToTypedArray") {
    val uint8Array = new Uint8Array(6)
    uint8Array(0) = 0xaa.toByte
    uint8Array(5) = 0xff.toByte
    hex"bbccdeadbeefee".copyToUint8Array(uint8Array, 1, 2, 4)
    assert(ByteVector.view(uint8Array) === hex"aadeadbeefff")
  }

  test("toTypedArray") {
    assert(ByteVector.view(hex"deadbeef".toUint8Array) === hex"deadbeef")
  }
}
