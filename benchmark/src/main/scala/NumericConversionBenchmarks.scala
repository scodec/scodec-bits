package scodec.bits

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{ Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State }

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class NumericConversionBenchmarks {

  val N = 1024

  def bitVectors(size: Int) = (0 to N).map { n => BitVector.fromLong(n.toLong).take(size.toLong).compact }

  val bitVectors64bits = bitVectors(64)
  val bitVectors60bits = bitVectors(60)
  val bitVectors32bits = bitVectors(32)
  val bitVectors32bitsNonCompacted = bitVectors(64).map { _.drop(32) }
  val bitVectors24bits = bitVectors(24)
  val bitVectors16bits = bitVectors(16)
  val bitVectors14bits = bitVectors(14)
  val bitVectors8bits = bitVectors(8)

  @Benchmark def toInt_32bit_bigEndian_nonCompacted =
    bitVectors32bitsNonCompacted.foldLeft(List[Int]()) { (acc, b) => b.toInt() :: acc }

  @Benchmark def toInt_32bit_littleEndian_nonCompacted =
    bitVectors32bitsNonCompacted.foldLeft(List[Int]()) { (acc, b) => b.toInt(ordering = ByteOrdering.LittleEndian) :: acc }

  @Benchmark def toInt_32bit_bigEndian =
    bitVectors32bits.foldLeft(List[Int]()) { (acc, b) => b.toInt() :: acc }

  @Benchmark def toInt_32bit_littleEndian =
    bitVectors32bits.foldLeft(List[Int]()) { (acc, b) => b.toInt(ordering = ByteOrdering.LittleEndian) :: acc }

  @Benchmark def toInt_16bit_bigEndian =
    bitVectors16bits.foldLeft(List[Int]()) { (acc, b) => b.toInt() :: acc }

  @Benchmark def toInt_16bit_littleEndian =
    bitVectors16bits.foldLeft(List[Int]()) { (acc, b) => b.toInt(ordering = ByteOrdering.LittleEndian) :: acc }

  @Benchmark def toInt_16bit_bigEndian_unsigned =
    bitVectors16bits.foldLeft(List[Int]()) { (acc, b) => b.toInt(signed = false) :: acc }

  @Benchmark def toInt_16bit_littleEndian_unsigned =
    bitVectors16bits.foldLeft(List[Int]()) { (acc, b) => b.toInt(signed = false, ordering = ByteOrdering.LittleEndian) :: acc }

  @Benchmark def toInt_24bit_bigEndian =
    bitVectors24bits.foldLeft(List[Int]()) { (acc, b) => b.toInt() :: acc }

  @Benchmark def toInt_14bit_bigEndian =
    bitVectors14bits.foldLeft(List[Int]()) { (acc, b) => b.toInt() :: acc }

  @Benchmark def toInt_8bit_bigEndian =
    bitVectors8bits.foldLeft(List[Int]()) { (acc, b) => b.toInt() :: acc }

  @Benchmark def toLong_64bit_bigEndian =
    bitVectors64bits.foldLeft(List[Long]()) { (acc, b) => b.toLong() :: acc }

  @Benchmark def toLong_64bit_littleEndian =
    bitVectors64bits.foldLeft(List[Long]()) { (acc, b) => b.toLong(ordering = ByteOrdering.LittleEndian) :: acc }

  @Benchmark def toLong_60bit_bigEndian =
    bitVectors60bits.foldLeft(List[Long]()) { (acc, b) => b.toLong() :: acc }

  @Benchmark def toLong_32bit_bigEndian =
    bitVectors32bits.foldLeft(List[Long]()) { (acc, b) => b.toLong() :: acc }

  val ints = (0 to N).toList

  @Benchmark def fromInt_32bit_bigEndian =
    ints.foldLeft(List[BitVector]()) { (acc, n) => BitVector.fromInt(n) :: acc }

  @Benchmark def fromInt_32bit_littleEndian =
    ints.foldLeft(List[BitVector]()) { (acc, n) => BitVector.fromInt(n, ordering = ByteOrdering.LittleEndian) :: acc }

  @Benchmark def fromInt_30bit_bigEndian =
    ints.foldLeft(List[BitVector]()) { (acc, n) => BitVector.fromInt(n, size = 30) :: acc }

  @Benchmark def fromInt_30bit_littleEndian =
    ints.foldLeft(List[BitVector]()) { (acc, n) => BitVector.fromInt(n, size = 30, ordering = ByteOrdering.LittleEndian) :: acc }

  @Benchmark def fromInt_16bit_bigEndian =
    ints.foldLeft(List[BitVector]()) { (acc, n) => BitVector.fromInt(n, size = 16) :: acc }
}
