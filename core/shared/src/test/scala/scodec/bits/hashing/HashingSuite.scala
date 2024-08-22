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
package hashing

import org.scalacheck.Prop.forAll

class HashingSuite extends BitsSuite with HashingSuitePlatform {
  property("md5 should always generate 128 bits") {
    forAll { (str: String) =>
      val Right(bv) = ByteVector.encodeUtf8(str): @unchecked
      assertEquals(bv.md5.bits.length, 128L)
    }
  }
  test("md5 of small strings") {
    val Right(empty) = ByteVector.encodeUtf8(""): @unchecked
    val Right(abc) = ByteVector.encodeUtf8("abc"): @unchecked
    val Right(digits) = ByteVector.encodeUtf8("0123456789"): @unchecked
    val Right(messageDigest) = ByteVector.encodeUtf8("message digest"): @unchecked
    val Right(alpha) = ByteVector.encodeUtf8("abcdefghijklmnopqrstuvwxyz"): @unchecked

    assertEquals(empty.md5, hex"d41d8cd98f00b204e9800998ecf8427e")
    assertEquals(abc.md5, hex"900150983cd24fb0d6963f7d28e17f72")
    assertEquals(digits.md5, hex"0x781e5e245d69b566979b86e28d23f2c7")
    assertEquals(messageDigest.md5, hex"0xf96b697d7cb7938d525a2f31aaf161d0")
    assertEquals(alpha.md5, hex"0xc3fcd3d76192e4007dfb496cca67e13b")
  }
  test("md5 of longer strings") {
    val bytes =
      hex"0x3138356239413133626134346332373634626438313162304143354130353943303832326633623332356445313436663861386145453438306143426135363465663834454536433336"
    // confirm bytes are valid utf-8
    assertEquals(
      bytes.decodeUtf8,
      Right("185b9A13ba44c2764bd811b0AC5A059C0822f3b325dE146f8a8aEE480aCBa564ef84EE6C36")
    )
    assertEquals(bytes.md5, hex"0xf9d5f1e5da8ba49e92bf6d18c787c5f4")
  }

  test("well-known md5 collision case") {
    val one =
      hex"""                        #...cab58...
      d131dd02c5e6eec4693d9a0698aff95c2fcab58712467eab4004583eb8fb7f89
      55ad340609f4b30283e488832571415a085125e8f7cdc99fd91dbdf280373c5b
      d8823e3156348f5bae6dacd436c919c6dd53e2b487da03fd02396306d248cda0
      e99f33420f577ee8ce54b67080a80d1ec69821bcb6a8839396f9652b6ff72a70
                        #...7080a...
      """
    val another =
      hex"""                        #...cab50...
      d131dd02c5e6eec4693d9a0698aff95c2fcab50712467eab4004583eb8fb7f89
      55ad340609f4b30283e4888325f1415a085125e8f7cdc99fd91dbd7280373c5b
      d8823e3156348f5bae6dacd436c919c6dd53e23487da03fd02396306d248cda0
      e99f33420f577ee8ce54b67080280d1ec69821bcb6a8839396f965ab6ff72a70
                        #...70802...
      """
    assertEquals(one.md5, another.md5)
  }

  property("sha1 should always generate 160 bits") {
    forAll { (str: String) =>
      val Right(bv) = ByteVector.encodeUtf8(str): @unchecked
      assertEquals(bv.sha1.bits.length, 160L)
    }
  }
  test("sha1 of small strings") {
    val Right(empty) = ByteVector.encodeUtf8(""): @unchecked
    val Right(abc) = ByteVector.encodeUtf8("abc"): @unchecked
    val Right(digits) = ByteVector.encodeUtf8("0123456789"): @unchecked
    assertEquals(empty.sha1, hex"da39a3ee5e6b4b0d3255bfef95601890afd80709")
    assertEquals(abc.sha1, hex"a9993e364706816aba3e25717850c26c9cd0d89d")
    assertEquals(digits.sha1, hex"87acec17cd9dcd20a716cc2cf67417b71c8a7016")
  }

  property("sha256 should always generate 160 bits") {
    forAll { (str: String) =>
      val Right(bv) = ByteVector.encodeUtf8(str): @unchecked
      assertEquals(bv.sha256.bits.length, 256L)
    }
  }

  test("sha256 of small strings") {
    val Right(empty) = ByteVector.encodeUtf8(""): @unchecked
    val Right(abc) = ByteVector.encodeUtf8("abc"): @unchecked
    val Right(digits) = ByteVector.encodeUtf8("0123456789"): @unchecked
    val Right(def_) = ByteVector.encodeUtf8("def"): @unchecked
    val Right(kevin) = ByteVector.encodeUtf8("Kevin"): @unchecked
    assertEquals(
      empty.sha256,
      hex"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
    assertEquals(
      abc.sha256,
      hex"0xba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
    assertEquals(
      digits.sha256,
      hex"84d89877f0d4041efb6bf91a16f0248f2fd573e6af05c19f96bedb9f882f7882"
    )
    assertEquals(
      def_.sha256,
      hex"cb8379ac2098aa165029e3938a51da0bcecfc008fd6795f401178647f96c5b34"
    )
    assertEquals(
      kevin.sha256,
      hex"0e4dd66217fc8d2e298b78c8cd9392870dcd065d0ff675d0edff5bcd227837e9"
    )
  }
  test("sha256 of large strings") {
    val Right(lorem) = ByteVector.encodeUtf8(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. " +
        "Proin pulvinar turpis purus, sit amet dapibus magna commodo " +
        "quis metus."
    ): @unchecked
    assertEquals(
      lorem.sha256,
      hex"60497604d2f6b4df42cea5efb8956f587f81a4ad66fa1b65d9e085224d255036"
    )
    val Right(million) = ByteVector.encodeUtf8("0" * 1000000): @unchecked
    assertEquals(
      million.sha256,
      hex"ba4b3010e2d91c08bd1987998d82b89b52ae1bdbc360f066607c7ee5a9c5830e"
    )
  }
  test("hmac") {
    val hmacKey = hex"0102030405060708"
    val Right(abc) = ByteVector.encodeUtf8("abc"): @unchecked
    assertEquals(
      abc.hmacSha1(hmacKey),
      hex"987af8649982ff7d9fbb1b8aa35099146997af51"
    )
    assertEquals(
      abc.hmacSha256(hmacKey),
      hex"446d1715583cf1c30dfffbec0df4ff1f9d39d493211ab4c97ed6f3f0eb579b47"
    )
  }
}
