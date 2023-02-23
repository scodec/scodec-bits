package scodec.bits

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class ByteVectorBenchmark {
  @Param(Array("1000", "10000", "100000"))
  var size: Int = _

  private var bv: ByteVector = _

  def bytes: Array[Byte] = {
    val r = new scala.util.Random(size)
    val bs = new Array[Byte](size * 3)
    r.nextBytes(bs)
    bs
  }

  @Setup(Level.Trial)
  def setup(): Unit = {
    bv = ByteVector.view(bytes)
  }

  @Benchmark
  def encodeHex: String =
    bv.toHex
}

