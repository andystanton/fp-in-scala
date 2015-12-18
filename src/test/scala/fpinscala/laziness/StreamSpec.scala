package fpinscala.laziness

import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {
  // exercise 5.1
  "A Stream" should "be converted to a List" in {
    val foo = Stream(1, 2, 3, 4, 5)
    foo.toList shouldBe List(1, 2, 3, 4, 5)
  }
}
