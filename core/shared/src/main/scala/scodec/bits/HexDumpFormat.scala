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

/** Creates hex dumps for bit and byte vectors.
  *
  * Formatting options can be specified by starting with `HexDumpFormat.Default`
  * and then calling various `withXyz` methods.
  */
final class HexDumpFormat private (
    val includeAddressColumn: Boolean,
    val dataColumnCount: Int,
    val dataColumnWidthInBytes: Int,
    val includeAsciiColumn: Boolean,
    val alphabet: Bases.HexAlphabet,
    val ansiEnabled: Boolean,
    val addressOffset: Int,
    val lengthLimit: Long
) {
  @inline private def numBytesPerLine = dataColumnWidthInBytes * dataColumnCount
  @inline private def numBitsPerLine = numBytesPerLine * 8L

  def withIncludeAddressColumn(newIncludeAddressColumn: Boolean): HexDumpFormat =
    new HexDumpFormat(
      newIncludeAddressColumn,
      dataColumnCount,
      dataColumnWidthInBytes,
      includeAsciiColumn,
      alphabet,
      ansiEnabled,
      addressOffset,
      lengthLimit
    )
  def withDataColumnCount(newDataColumnCount: Int): HexDumpFormat =
    new HexDumpFormat(
      includeAddressColumn,
      newDataColumnCount,
      dataColumnWidthInBytes,
      includeAsciiColumn,
      alphabet,
      ansiEnabled,
      addressOffset,
      lengthLimit
    )
  def withDataColumnWidthInBytes(newDataColumnWidthInBytes: Int): HexDumpFormat =
    new HexDumpFormat(
      includeAddressColumn,
      dataColumnCount,
      newDataColumnWidthInBytes,
      includeAsciiColumn,
      alphabet,
      ansiEnabled,
      addressOffset,
      lengthLimit
    )
  def withIncludeAsciiColumn(newIncludeAsciiColumn: Boolean): HexDumpFormat =
    new HexDumpFormat(
      includeAddressColumn,
      dataColumnCount,
      dataColumnWidthInBytes,
      newIncludeAsciiColumn,
      alphabet,
      ansiEnabled,
      addressOffset,
      lengthLimit
    )
  def withAlphabet(newAlphabet: Bases.HexAlphabet): HexDumpFormat =
    new HexDumpFormat(
      includeAddressColumn,
      dataColumnCount,
      dataColumnWidthInBytes,
      includeAsciiColumn,
      newAlphabet,
      ansiEnabled,
      addressOffset,
      lengthLimit
    )
  def withAnsi(newAnsiEnabled: Boolean): HexDumpFormat =
    new HexDumpFormat(
      includeAddressColumn,
      dataColumnCount,
      dataColumnWidthInBytes,
      includeAsciiColumn,
      alphabet,
      newAnsiEnabled,
      addressOffset,
      lengthLimit
    )
  def withAddressOffset(newAddressOffset: Int): HexDumpFormat =
    new HexDumpFormat(
      includeAddressColumn,
      dataColumnCount,
      dataColumnWidthInBytes,
      includeAsciiColumn,
      alphabet,
      ansiEnabled,
      newAddressOffset,
      lengthLimit
    )
  def withLengthLimit(newLengthLimit: Long): HexDumpFormat =
    new HexDumpFormat(
      includeAddressColumn,
      dataColumnCount,
      dataColumnWidthInBytes,
      includeAsciiColumn,
      alphabet,
      ansiEnabled,
      addressOffset,
      newLengthLimit
    )

  def render(bytes: ByteVector): String =
    render(bytes.bits)

  def render(bits: => BitVector): String = {
    val bldr = new StringBuilder
    render(bits, line => { bldr.append(line); () })
    bldr.toString
  }

  def render(bits: => BitVector, onLine: String => Unit): Unit =
    render(bits, 0L, onLine)

  @annotation.tailrec
  private def render(bits: BitVector, position: Long, onLine: String => Unit): Unit = {
    // Note: we don't use grouped(numBitsPerLine) here to avoid holding on to a reference to original vector
    val takeFullLine = position + numBytesPerLine <= lengthLimit
    val bitsToTake = if (takeFullLine) numBitsPerLine else (lengthLimit - position) * 8L
    if (bits.nonEmpty && bitsToTake > 0L) {
      val bitsInLine = bits.take(bitsToTake)
      val line = renderLine(bitsInLine.bytes, (addressOffset + position).toInt)
      onLine(line)
      if (takeFullLine)
        render(bits.drop(numBitsPerLine), position + bitsInLine.size / 8, onLine)
    }
  }

  def print(bytes: ByteVector): Unit =
    print(bytes.bits)

  def print(bits: => BitVector): Unit =
    render(bits, line => Console.print(line))

  private object Ansi {
    val Faint = "\u001b[;2m"
    val Normal = "\u001b[;22m"
    val Reset = "\u001b[0m"
    def foregroundColor(bldr: StringBuilder, rgb: (Int, Int, Int)): Unit = {
      bldr
        .append("\u001b[38;2;")
        .append(rgb._1)
        .append(";")
        .append(rgb._2)
        .append(";")
        .append(rgb._3)
        .append("m")
      ()
    }
  }

  private def renderLine(bytes: ByteVector, address: Int): String = {
    val bldr = new StringBuilder
    if (includeAddressColumn) {
      if (ansiEnabled) bldr.append(Ansi.Faint)
      bldr.append(ByteVector.fromInt(address).toHex(alphabet))
      if (ansiEnabled) bldr.append(Ansi.Normal)
      bldr.append("  ")
    }
    bytes.grouped(dataColumnWidthInBytes.toLong).foreach { columnBytes =>
      renderHex(bldr, columnBytes)
      bldr.append(" ")
    }
    if (ansiEnabled)
      bldr.append(Ansi.Reset)
    if (includeAsciiColumn) {
      val padding = {
        val bytesOnThisLine = bytes.size.toInt
        val dataBytePadding = (numBytesPerLine - bytesOnThisLine) * 3 - 1
        val numFullDataColumns = (bytesOnThisLine - 1) / dataColumnWidthInBytes
        val numAdditionalColumnSpacers = dataColumnCount - numFullDataColumns
        dataBytePadding + numAdditionalColumnSpacers
      }
      bldr.append(" " * padding)
      bldr.append('│')
      renderAsciiBestEffort(bldr, bytes)
      bldr.append('│')
    }
    bldr.append('\n')
    bldr.toString
  }

  private def renderHex(bldr: StringBuilder, bytes: ByteVector): Unit =
    bytes.foreachS {
      new ByteVector.F1BU {
        def apply(b: Byte): Unit = {
          if (ansiEnabled) Ansi.foregroundColor(bldr, rgbForByte(b))
          bldr
            .append(alphabet.toChar((b >> 4 & 0x0f).toByte.toInt))
            .append(alphabet.toChar((b & 0x0f).toByte.toInt))
            .append(' ')
          ()
        }
      }
    }

  private def rgbForByte(b: Byte): (Int, Int, Int) = {
    val saturation = 0.4
    val value = 0.75
    val hue = ((b & 0xff) / 256.0) * 360.0
    hsvToRgb(hue, saturation, value)
  }

  // From https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB
  private def hsvToRgb(hue: Double, saturation: Double, value: Double): (Int, Int, Int) = {
    val c = saturation * value
    val h = hue / 60
    val x = c * (1 - (h % 2 - 1).abs)
    val z = 0d
    val (r1, g1, b1) = h.toInt match {
      case 0 => (c, x, z)
      case 1 => (x, c, z)
      case 2 => (z, c, x)
      case 3 => (z, x, c)
      case 4 => (x, z, c)
      case 5 => (c, z, x)
    }
    val m = value - c
    val (r, g, b) = (r1 + m, g1 + m, b1 + m)
    def scale(v: Double) = (v * 256).toInt
    (scale(r), scale(g), scale(b))
  }

  private val FaintDot = s"${Ansi.Faint}.${Ansi.Normal}"
  private val FaintUnmappable = s"${Ansi.Faint}�${Ansi.Normal}"
  private val NonPrintablePattern = "[^�\\p{Print}]".r

  private def renderAsciiBestEffort(bldr: StringBuilder, bytes: ByteVector): Unit = {
    val decoded = bytes.decodeAsciiLenient
    val nonPrintableReplacement = if (ansiEnabled) FaintDot else "."
    val printable = NonPrintablePattern.replaceAllIn(decoded, nonPrintableReplacement)
    val colorized = if (ansiEnabled) printable.replaceAll("�", FaintUnmappable) else printable
    bldr.append(colorized)
    ()
  }
}

object HexDumpFormat {

  /** Colorized hex dump that displays 2 columns of 8 bytes each, along with the address column and ASCII column. */
  val Default: HexDumpFormat =
    new HexDumpFormat(true, 2, 8, true, Bases.Alphabets.HexLowercase, true, 0, Long.MaxValue)

  /** Like [[Default]] but with ANSI color disabled. */
  val NoAnsi: HexDumpFormat =
    Default.withAnsi(false)

  /** Like [[Default]] but with 3 columns of data and no ASCII column. */
  val NoAscii: HexDumpFormat =
    Default.withIncludeAsciiColumn(false).withDataColumnCount(3)
}
