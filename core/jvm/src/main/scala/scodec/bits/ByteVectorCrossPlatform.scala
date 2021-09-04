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
import java.util.zip.{DataFormatException, Deflater, Inflater}

import javax.crypto.Cipher

private[bits] trait ByteVectorCrossPlatform { self: ByteVector =>

  /** Compresses this vector using ZLIB.
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
  ): ByteVector =
    if (isEmpty) this
    else {
      val deflater = new Deflater(level, nowrap)
      try {
        deflater.setStrategy(strategy)

        val buffer = new Array[Byte](chunkSize.toLong.min(size).toInt)
        def loop(acc: ByteVector, fin: Boolean): ByteVector =
          if ((fin && deflater.finished) || (!fin && deflater.needsInput)) acc
          else {
            val count = deflater.deflate(buffer)
            loop(acc ++ ByteVector(buffer, 0, count), fin)
          }

        var result = ByteVector.empty

        foreachV { v =>
          deflater.setInput(v.toArray)
          result = result ++ loop(ByteVector.empty, false)
        }

        deflater.setInput(Array.empty[Byte])
        deflater.finish()
        result ++ loop(ByteVector.empty, true)
      } finally deflater.end()
    }

  /** Decompresses this vector using ZLIB.
    *
    * @param chunkSize
    *   buffer size, in bytes, to use when compressing
    * @param nowrap
    *   if true, will assume no ZLIB header and checksum
    * @group conversions
    */
  final def inflate(
      chunkSize: Int = 4096,
      nowrap: Boolean = false
  ): Either[DataFormatException, ByteVector] =
    if (isEmpty) Right(this)
    else {
      val arr = toArray

      val inflater = new Inflater(nowrap)
      try {
        inflater.setInput(arr)
        try {
          val buffer = new Array[Byte](chunkSize.min(arr.length))
          def loop(acc: ByteVector): ByteVector =
            if (inflater.finished || inflater.needsInput) acc
            else {
              val count = inflater.inflate(buffer)
              loop(acc ++ ByteVector(buffer, 0, count))
            }
          val inflated = loop(ByteVector.empty)
          if (inflater.finished) Right(inflated)
          else
            Left(
              new DataFormatException(
                "Insufficient data -- inflation reached end of input without completing inflation - " + inflated
              )
            )
        } catch {
          case e: DataFormatException => Left(e)
        }
      } finally inflater.end()
    }

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
