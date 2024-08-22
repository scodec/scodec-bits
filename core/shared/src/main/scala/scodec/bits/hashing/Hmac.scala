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

import scodec.bits.internal.syntax.*

private[bits] class Hmac private (
    private val hasher: Hasher,
    private val outerKey: Array[Byte]
) extends Hasher {
  override def update(input: Array[Byte], offset: Int, byteCount: Int): Unit =
    hasher.update(input, offset, byteCount)

  override def digest(): Array[Byte] = {
    val digest = hasher.digest()

    hasher.update(outerKey, 0, outerKey.size)
    hasher.update(digest, 0, digest.size)

    hasher.digest()
  }
}

object Hmac {
  private val IPAD: Byte = 54
  private val OPAD: Byte = 92

  def sha1(key: ByteVector): Hmac =
    create(key, hasher = new Sha1(), blockLength = 64)

  def sha256(key: ByteVector): Hmac =
    create(key, hasher = new Sha256(), blockLength = 64)

  private def create(
      key: ByteVector,
      hasher: Hasher,
      blockLength: Int
  ): Hmac = {
    val keySize = key.size
    val paddedKey =
      if (keySize == 0) throw new IllegalArgumentException("Empty key")
      else if (keySize == blockLength) key.toArray
      else if (keySize < blockLength) key.toArray.copyOf(blockLength)
      else { hasher.update(key.toArray, 0, key.size.toInt); hasher.digest() }.copyOf(blockLength)
    val innerKey = Array.tabulate[Byte](blockLength)(it => (paddedKey(it) ^ IPAD).toByte)
    val outerKey = Array.tabulate(blockLength)(it => (paddedKey(it) ^ OPAD).toByte)

    hasher.update(innerKey, 0, innerKey.size)

    return new Hmac(
      hasher,
      outerKey
    )
  }
}
