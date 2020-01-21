package scodec.bits

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import Matchers._

abstract class BitsSuite extends AnyFunSuite with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100, workers = 4)

  protected def serializationShouldRoundtrip[A](x: A): Unit = {
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
    deserialized shouldBe x
    ()
  }
}
