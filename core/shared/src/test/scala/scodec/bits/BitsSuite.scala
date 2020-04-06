package scodec.bits

import munit.ScalaCheckSuite

abstract class BitsSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(100)
      .withWorkers(4)

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
    assertEquals(deserialized, x)
  }
}
