package fpinscala.datastructures

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

  "A List companion object" should "calculate the sum of a list of int values" in {
    List.sum(List(1, 2, 3)) shouldBe 6
  }

  it should "calculate the product of a list of double values" in {
    List.product(List(1.0, 2.0, 3.0)) shouldBe 6.0
  }

  it should "append two lists together" in {
    List.append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
    List.append(Nil, List(1, 2, 3)) shouldBe List(1, 2, 3)
    List.append(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
  }

  it should "foldRight over a list" in {
    List.foldRight(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
    List.foldRight(List(1, 2, 3, 4), 1)(_ * _) shouldBe 24
  }

  // exercise 3.2
  it should "return the tail of a list" in {
    List.tail(List(1, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5)
    List.tail(List(1)) shouldBe Nil

    intercept[IllegalArgumentException] {
      List.tail(Nil)
    }.getMessage shouldBe "Cannot return tail of nil"
  }

  // exercise 3.3
  it should "set the head of a list" in {
    List.setHead(List(1, 2, 3, 4, 5), 8) shouldBe List(8, 2, 3, 4, 5)
    List.setHead(List(3), 4) shouldBe List(4)

    intercept[IllegalArgumentException] {
      List.setHead(Nil, 3)
    }.getMessage shouldBe "Cannot set head of nil"
  }

  // exercise 3.4
  it should "drop the first n elements of a list" in {
    List.drop(List(1, 2, 3, 4, 5, 6), 3) shouldBe List(4, 5, 6)
    List.drop(List(1), 1) shouldBe Nil

    intercept[IllegalArgumentException] {
      List.drop(Nil, 2)
    }.getMessage shouldBe "Cannot drop elements from nil"
  }

  // exercise 3.5
  it should "drop the first elements that satisfy a predicate" in {
    List.dropWhile(List(1, 2, 3, 4, 5, 6), (x: Int) => x < 4) shouldBe List(4, 5, 6)
    List.dropWhile(List(1), (x: Int) => x < 3) shouldBe Nil

    intercept[IllegalArgumentException] {
      List.dropWhile(Nil, (x: Int) => x < 5)
    }.getMessage shouldBe "Cannot drop elements from nil"

    List.dropWhile2(List(1, 2, 3, 4, 5, 6))(x => x < 4) shouldBe List(4, 5, 6)
    List.dropWhile2(List(1))(x => x < 3) shouldBe Nil

    intercept[IllegalArgumentException] {
      List.dropWhile2(Nil: List[Int])(x => x < 5)
    }.getMessage shouldBe "Cannot drop elements from nil"
  }

  // exercise 3.6
  it should "return all but the last element of a list" in {
    List.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
    intercept[IllegalArgumentException] {
      List.init(List(1))
    }.getMessage shouldBe "Cannot return init of list containing single item"
    intercept[IllegalArgumentException] {
      List.init(Nil)
    }.getMessage shouldBe "Cannot return init of nil"
  }
}
