package fpinscala.laziness

import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {
  def shouldBeSameStream(a: Stream[Any], b: Stream[Any]) = a === b

  // exercise 5.1
  "A Stream" should "be converted to a List" in {
    val foo = Stream(1, 2, 3, 4, 5)
    foo.toList shouldBe List(1, 2, 3, 4, 5)
  }

  it should "be converted to a List via recursion" in {
    val foo = Stream(1, 2, 3, 4, 5)
    foo.toListViaRecursion shouldBe List(1, 2, 3, 4, 5)
  }

  it should "be comparable to another Stream" in {
    // same type & equal
    Stream() === Stream() shouldBe true
    Stream(1) === Stream(1) shouldBe true
    Stream(1, 2, 3, 4, 5) === Stream(1, 2, 3, 4, 5) shouldBe true

    // same type & not equal
    Stream() === Stream(1) shouldBe false
    Stream() === Stream(1, 2, 3, 4, 5) shouldBe false

    // not same type
    Stream(1) === Stream("1") shouldBe false
  }

  it should "reverse a Stream" in {
    Stream(1, 2, 3, 4, 5).reverse === Stream(5, 4, 3, 2, 1) shouldBe true
  }

  // exercise 5.2
  it should "take the first n elements" in {
    Stream(1, 2, 3, 4, 5).take(3) === Stream(1, 2, 3) shouldBe true
  }

  it should "drop the first n elements" in {
    Stream(1, 2, 3, 4, 5).drop(3) === Stream(4, 5) shouldBe true
  }
}
