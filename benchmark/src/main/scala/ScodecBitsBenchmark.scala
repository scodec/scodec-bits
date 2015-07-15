package scodec.bits

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{ Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State }
import akka.util.ByteString

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ScodecBitsBenchmark {

  val N = 100000L
  val M = 1024L

  val bitChunks_N = (0L until N).map(b => BitVector(b.toByte)).toList
  val byteChunks_N = (0L until N).map(b => ByteVector(b.toByte)).toList
  val byteStringChunks_N = (0L until N).map(b => ByteString(b.toByte)).toList
  val bytes_N = Array.tabulate(N.toInt)(i => i.toByte)
  val bitVector_N = bitChunks_N.foldLeft(BitVector.empty)(_ ++ _)
  val byteVector_N = byteChunks_N.foldLeft(ByteVector.empty)(_ ++ _)
  val byteString_N = byteStringChunks_N.foldLeft(ByteString())(_ ++ _)

  val bitChunks_M = (0L until M).map(b => BitVector(b.toByte)).toList
  val byteChunks_M = (0L until M).map(b => ByteVector(b.toByte)).toList
  val byteStringChunks_M = (0L until M).map(b => ByteString(b.toByte)).toList
  val bytes_M = Array.tabulate(M.toInt)(i => i.toByte)
  val bitVector_M = bitChunks_M.foldLeft(BitVector.empty)(_ ++ _)
  val bitVector_M_compact = bitVector_M.copy
  val byteVector_M = byteChunks_M.foldLeft(ByteVector.empty)(_ ++ _)
  val byteString_M = byteStringChunks_M.foldLeft(ByteString())(_ ++ _)

  @Benchmark def listCons_N(): Int =
    bitChunks_N.foldLeft(List[BitVector]())((t,h) => h :: t).size
  @Benchmark def vectorSnoc_N(): Int =
    bitChunks_N.foldLeft(Vector[BitVector]())((a,b) => a :+ b).size
  @Benchmark def listCons_M(): Int =
    bitChunks_M.foldLeft(List[BitVector]())((t,h) => h :: t).size
  @Benchmark def vectorSnoc_M(): Int =
    bitChunks_M.foldLeft(Vector[BitVector]())((a,b) => a :+ b).size

  // N
  @Benchmark def bitVectorAppendSnoc_N(): Long =
    bitChunks_N.foldLeft(BitVector.empty)(_ ++ _).size
  @Benchmark def byteVectorAppendSnoc_N(): Int =
    byteChunks_N.foldLeft(ByteVector.empty)(_ ++ _).size
  @Benchmark def byteVectorSnoc_N(): Int =
    bytes_N.foldLeft(ByteVector.empty)(_ :+ _).size
  @Benchmark def byteVectorSnocUnboxed_N(): Int = {
    var b = ByteVector.empty
    var i = 0
    while (i < bytes_N.length) {
      b = b :+ bytes_N(i)
      i += 1
    }
    b.size
  }
  @Benchmark def byteStringAppendSnoc_N(): Int =
    byteStringChunks_N.foldLeft(ByteString())(_ ++ _).size
  @Benchmark def byteStringSnoc_N(): Int =
    bytes_N.foldLeft(ByteString())(_ :+ _).size
  @Benchmark def byteStringSnocUnboxed_N(): Int = {
    var b = ByteString()
    var i = 0
    while (i < bytes_N.length) {
      b = b :+ bytes_N(i)
      i += 1
    }
    b.size
  }
  @Benchmark def bitVectorStride_N(): Long =
    (0L until (N/512)).foldLeft(bitVector_N)((b,_) => b.drop(512*8)).size
  @Benchmark def byteVectorStride_N(): Int =
    (0L until (N/512)).foldLeft(byteVector_N)((b,_) => b.drop(512)).size
  @Benchmark def byteStringStride_N(): Int =
    (0L until (N/512)).foldLeft(byteString_N)((b,_) => b.drop(512)).size
  @Benchmark def bitVectorTake_N(): Long =
    (N until 0L by -512L).foldLeft(bitVector_N)((b,n) => b.take(n)).size
  @Benchmark def byteVectorTake_N(): Int =
    (N.toInt until 0 by -512).foldLeft(byteVector_N)((b,n) => b.take(n)).size
  @Benchmark def byteStringTake_N(): Int =
    (N.toInt until 0 by -512).foldLeft(byteString_N)((b,n) => b.take(n)).size

  // M
  @Benchmark def bitVectorAppendSnoc_M(): Long =
    bitChunks_M.foldLeft(BitVector.empty)(_ ++ _).size
  @Benchmark def byteVectorAppendSnoc_M(): Int =
    byteChunks_M.foldLeft(ByteVector.empty)(_ ++ _).size
  @Benchmark def byteVectorSnoc_M(): Int =
    bytes_M.foldLeft(ByteVector.empty)(_ :+ _).size
  @Benchmark def byteVectorSnocUnboxed_M(): Int = {
    var b = ByteVector.empty
    var i = 0
    while (i < bytes_M.length) {
      b = b :+ bytes_M(i)
      i += 1
    }
    b.size
  }
  @Benchmark def byteStringAppendSnoc_M(): Int =
    byteStringChunks_M.foldLeft(ByteString())(_ ++ _).size
  @Benchmark def byteStringSnoc_M(): Int =
    bytes_M.foldLeft(ByteString())(_ :+ _).size
  @Benchmark def byteStringSnocUnboxed_M(): Int = {
    var b = ByteString()
    var i = 0
    while (i < bytes_M.length) {
      b = b :+ bytes_M(i)
      i += 1
    }
    b.size
  }
  @Benchmark def bitVectorStride_M(): Long =
    (0L until (M/512)).foldLeft(bitVector_M)((b,_) => b.drop(512*8)).size
  @Benchmark def byteVectorStride_M(): Int =
    (0L until (M/512)).foldLeft(byteVector_M)((b,_) => b.drop(512)).size
  @Benchmark def byteStringStride_M(): Int =
    (0L until (M/512)).foldLeft(byteString_M)((b,_) => b.drop(512)).size
  @Benchmark def bitVectorTake_M(): Long =
    (M until 0L by -512L).foldLeft(bitVector_M)((b,n) => b.take(n)).size
  @Benchmark def byteVectorTake_M(): Int =
    (M.toInt until 0 by -512).foldLeft(byteVector_M)((b,n) => b.take(n)).size
  @Benchmark def byteStringTake_M(): Int =
    (M.toInt until 0 by -512).foldLeft(byteString_M)((b,n) => b.take(n)).size

  @Benchmark def toBase64(): String =
    bitVector_M.toBase64
  @Benchmark def toBase64_compact(): String =
    bitVector_M_compact.toBase64
  @Benchmark def toBase64_JRE(): String =
    java.util.Base64.getEncoder.encodeToString(bitVector_M.toByteArray)
  @Benchmark def toBase64_JRE_compact(): String =
    java.util.Base64.getEncoder.encodeToString(bitVector_M_compact.toByteArray)
}
