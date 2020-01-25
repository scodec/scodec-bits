package scodec.bits

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

abstract class BitsSuite extends AnyFunSuite with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100, workers = 4)

  protected def serializationShouldRoundtrip[A](x: A): Assertion = {
    import java.io.{
      ByteArrayInputStream,
      ByteArrayOutputStream,
      ObjectInputStream,
      ObjectOutputStream
    }
    val bout = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bout)
    out.writeObject(x)
    out.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bout.toByteArray))
    val deserialized = in.readObject.asInstanceOf[A]
    assert(deserialized == x)
  }
}
