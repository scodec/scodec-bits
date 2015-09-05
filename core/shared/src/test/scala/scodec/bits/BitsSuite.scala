package scodec.bits

import org.scalatest.{ FunSuite, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import Matchers._

abstract class BitsSuite extends FunSuite with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100, workers = 4)

  protected def serializationShouldRoundtrip[A](x: A): Unit = {
    import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream }
    val bout = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bout)
    out.writeObject(x)
    out.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bout.toByteArray))
    val deserialized = in.readObject.asInstanceOf[A]
    deserialized shouldBe x
  }
}
