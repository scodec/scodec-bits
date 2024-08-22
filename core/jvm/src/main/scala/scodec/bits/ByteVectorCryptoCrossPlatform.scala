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

import javax.crypto.{Cipher, Mac}
import javax.crypto.spec.SecretKeySpec
import java.security.{
  AlgorithmParameters,
  GeneralSecurityException,
  InvalidKeyException,
  Key,
  MessageDigest,
  SecureRandom
}

private[bits] trait ByteVectorCryptoCrossPlatform { self: ByteVector =>

  /** Computes a SHA-1 digest of this byte vector.
    * @group conversions
    */
  final def sha1: ByteVector = digest("SHA-1")

  /** Computes a SHA-256 digest of this byte vector.
    * @group conversions
    */
  final def sha256: ByteVector = digest("SHA-256")

  /** Computes an MD5 digest of this byte vector.
    * @group conversions
    */
  final def md5: ByteVector = digest("MD5")

  /** Computes a digest of this byte vector.
    * @param algorithm
    *   digest algorithm to use
    * @group conversions
    */
  final def digest(algorithm: String): ByteVector = digest(MessageDigest.getInstance(algorithm))

  /** Computes a digest of this byte vector.
    * @param digest
    *   digest to use
    * @group conversions
    */
  final def digest(digest: MessageDigest): ByteVector = {
    foreachV { v =>
      digest.update(v.toArray)
    }
    ByteVector.view(digest.digest)
  }

  /** Returns the 160-bit SHA-1 HMAC of this buffer. */
  final def hmacSha1(key: ByteVector) = hmac("HmacSHA1", key)

  /** Returns the 256-bit SHA-256 HMAC of this buffer. */
  final def hmacSha256(key: ByteVector) = hmac("HmacSHA256", key)

  /** Returns the 512-bit SHA-512 HMAC of this buffer. */
  final def hmacSha512(key: ByteVector) = hmac("HmacSHA512", key)

  private def hmac(algorithm: String, key: ByteVector): ByteVector =
    try {
      val mac = Mac.getInstance(algorithm)
      mac.init(new SecretKeySpec(key.toArrayUnsafe, algorithm))
      foreachV { v =>
        mac.update(v.toArray, v.offset.toInt, (v.size - v.offset).toInt)
      }
      ByteVector(mac.doFinal())
    } catch {
      case e: InvalidKeyException =>
        throw new IllegalArgumentException(e)
    }

  /** Encrypts this byte vector using the specified cipher and key.
    *
    * @param ci
    *   cipher to use for encryption
    * @param key
    *   key to encrypt with
    * @param aparams
    *   optional algorithm paramaters used for encryption (e.g., initialization vector)
    * @param sr
    *   secure random
    * @group crypto
    */
  final def encrypt(ci: Cipher, key: Key, aparams: Option[AlgorithmParameters] = None)(implicit
      sr: SecureRandom
  ): Either[GeneralSecurityException, ByteVector] =
    cipher(ci, key, Cipher.ENCRYPT_MODE, aparams)

  /** Decrypts this byte vector using the specified cipher and key.
    *
    * @param ci
    *   cipher to use for decryption
    * @param key
    *   key to decrypt with
    * @param aparams
    *   optional algorithm paramaters used for decryption (e.g., initialization vector)
    * @param sr
    *   secure random
    * @group crypto
    */
  final def decrypt(ci: Cipher, key: Key, aparams: Option[AlgorithmParameters] = None)(implicit
      sr: SecureRandom
  ): Either[GeneralSecurityException, ByteVector] =
    cipher(ci, key, Cipher.DECRYPT_MODE, aparams)

  private[bits] def cipher(
      ci: Cipher,
      key: Key,
      opmode: Int,
      aparams: Option[AlgorithmParameters] = None
  )(implicit sr: SecureRandom): Either[GeneralSecurityException, ByteVector] =
    try {
      aparams.fold(ci.init(opmode, key, sr))(aparams => ci.init(opmode, key, aparams, sr))
      foreachV { view =>
        ci.update(view.toArrayUnsafe); ()
      }
      Right(ByteVector.view(ci.doFinal()))
    } catch {
      case e: GeneralSecurityException => Left(e)
    }

}
