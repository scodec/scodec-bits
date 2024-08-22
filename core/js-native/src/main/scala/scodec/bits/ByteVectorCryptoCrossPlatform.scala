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
import scodec.bits.hashing.*

private[bits] trait ByteVectorCryptoCrossPlatform { self: ByteVector =>

  def md5: ByteVector = {
    val hasher = new Md5()
    foreachV { v =>
      hasher.update(v.toArray, v.offset.toInt, (v.size - v.offset).toInt)
    }
    ByteVector.view(hasher.digest())
  }

  def sha1: ByteVector = {
    val hasher = new Sha1()
    foreachV { v =>
      hasher.update(v.toArray, v.offset.toInt, (v.size - v.offset).toInt)
    }
    ByteVector.view(hasher.digest())
  }

  def sha256: ByteVector = {
    val hasher = new Sha256()
    foreachV { v =>
      hasher.update(v.toArray, v.offset.toInt, (v.size - v.offset).toInt)
    }
    ByteVector.view(hasher.digest())
  }

  private def digest(hasher: Hasher): ByteVector = {
    foreachV { v =>
      hasher.update(v.toArray, v.offset.toInt, (v.size - v.offset).toInt)
    }

    return ByteVector(hasher.digest())
  }

  /** Returns the 160-bit SHA-1 HMAC of this buffer. */
  final def hmacSha1(key: ByteVector) = digest(Hmac.sha1(key))

  /** Returns the 256-bit SHA-256 HMAC of this buffer. */
  final def hmacSha256(key: ByteVector) = digest(Hmac.sha256(key))
}
