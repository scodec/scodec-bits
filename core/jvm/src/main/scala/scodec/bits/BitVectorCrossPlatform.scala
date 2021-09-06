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

import java.security.{
  AlgorithmParameters,
  GeneralSecurityException,
  Key,
  MessageDigest,
  SecureRandom
}
import java.util.zip.{DataFormatException, Deflater}
import javax.crypto.Cipher

private[bits] trait BitVectorCrossPlatform { self: BitVector =>

  /** Compresses this vector using ZLIB.
    *
    * The last byte is zero padded if the size is not evenly divisible by 8.
    *
    * @param level
    *   compression level, 0-9, with 0 disabling compression and 9 being highest level of
    *   compression -- see `java.util.zip.Deflater` for details
    * @param strategy
    *   compression strategy -- see `java.util.zip.Deflater` for details
    * @param nowrap
    *   if true, ZLIB header and checksum will not be used
    * @param chunkSize
    *   buffer size, in bytes, to use when compressing
    * @group conversions
    */
  final def deflate(
      level: Int = Deflater.DEFAULT_COMPRESSION,
      strategy: Int = Deflater.DEFAULT_STRATEGY,
      nowrap: Boolean = false,
      chunkSize: Int = 4096
  ): BitVector =
    bytes.deflate(level, strategy, nowrap, chunkSize).bits

  /** Decompresses this vector using ZLIB.
    *
    * The last byte is zero padded if the size is not evenly divisible by 8.
    *
    * @param chunkSize
    *   buffer size, in bytes, to use when compressing
    * @group conversions
    */
  final def inflate(chunkSize: Int = 4096): Either[DataFormatException, BitVector] =
    bytes.inflate(chunkSize).map(_.bits)

  /** Computes a SHA-1 digest of this bit vector.
    *
    * Exceptions thrown from the underlying JCA API are propagated.
    *
    * The last byte is zero padded if the size is not evenly divisible by 8.
    *
    * @group crypto
    */
  final def sha1: BitVector = digest("SHA-1")

  /** Computes a SHA-256 digest of this bit vector.
    *
    * Exceptions thrown from the underlying JCA API are propagated.
    *
    * The last byte is zero padded if the size is not evenly divisible by 8.
    *
    * @group crypto
    */
  final def sha256: BitVector = digest("SHA-256")

  /** Computes an MD5 digest of this bit vector.
    *
    * Exceptions thrown from the underlying JCA API are propagated.
    *
    * The last byte is zero padded if the size is not evenly divisible by 8.
    *
    * @group crypto
    */
  final def md5: BitVector = digest("MD5")

  /** Computes a digest of this bit vector.
    *
    * Exceptions thrown from the underlying JCA API are propagated.
    *
    * The last byte is zero padded if the size is not evenly divisible by 8.
    *
    * @param algorithm
    *   digest algorithm to use
    * @group crypto
    */
  final def digest(algorithm: String): BitVector = digest(MessageDigest.getInstance(algorithm))

  /** Computes a digest of this bit vector.
    *
    * Exceptions thrown from the underlying JCA API are propagated.
    *
    * The last byte is zero padded if the size is not evenly divisible by 8.
    *
    * @param digest
    *   digest to use
    * @group crypto
    */
  final def digest(digest: MessageDigest): BitVector = BitVector(bytes.digest(digest))

  /** Encrypts this bit vector using the specified cipher and key.
    *
    * The last byte is zero padded if the size is not evenly divisible by 8.
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
  ): Either[GeneralSecurityException, BitVector] =
    cipher(ci, key, Cipher.ENCRYPT_MODE, aparams)(sr)

  /** Decrypts this bit vector using the specified cipher and key.
    *
    * The last byte is zero padded if the size is not evenly divisible by 8.
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
  ): Either[GeneralSecurityException, BitVector] =
    cipher(ci, key, Cipher.DECRYPT_MODE, aparams)(sr)

  private[bits] def cipher(
      ci: Cipher,
      key: Key,
      opmode: Int,
      aparams: Option[AlgorithmParameters] = None
  )(implicit sr: SecureRandom): Either[GeneralSecurityException, BitVector] =
    bytes.cipher(ci, key, opmode, aparams)(sr).map(_.bits)

}
