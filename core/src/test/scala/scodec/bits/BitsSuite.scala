package scodec.bits

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class BitsSuite extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {

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
