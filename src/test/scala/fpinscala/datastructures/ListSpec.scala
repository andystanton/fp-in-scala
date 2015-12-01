package fpinscala.datastructures

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

  "A List companion object" should "construct a List" in {
    List(1, 2, 3, 4) shouldBe Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  }

  it should "calculate the length of a list" in {
    List.length(List(1, 2, 3, 4)) shouldBe 4
    List.length(List(1)) shouldBe 1
    List.length(Nil) shouldBe 0
  }

  it should "calculate the sum of a list of int values" in {
    List.sum(List(1, 2, 3, 4)) shouldBe 10
  }

  it should "calculate the sum of a list of int values using foldRight" in {
    List.sum2(List(1, 2, 3, 4)) shouldBe 10
  }

  it should "calculate the product of a list of double values" in {
    List.product(List(1.0, 2.0, 3.0, 4.0)) shouldBe 24.0
  }

  it should "calculate the product of a list of double values using foldRight" in {
    List.product2(List(1.0, 2.0, 3.0, 4.0)) shouldBe 24.0
  }

  it should "append two lists together" in {
    List.append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
    List.append(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
    List.append(Nil, List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  it should "foldRight over a list" in {
    List.foldRight(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
    List.foldRight(List(1, 2, 3, 4), 1)(_ * _) shouldBe 24
    List.foldRight(List(1, 2, 3, 4), 0)((_, b) => b + 1) shouldBe 4
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

  // exercise 3.7
  it should "reconstuct a list when foldRight is supplied with the list data constructor" in {
    List.foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3, 4)
  }

  // exercise 3.8
  it should "calculate the length of a list using foldRight" in {
    List.length2(List(1, 2, 3, 4)) shouldBe 4
    List.length2(List(1)) shouldBe 1
    List.length2(Nil) shouldBe 0
  }

  // exercise 3.10
  it should "foldLeft over a list" in {
    List.foldLeft(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
    List.foldLeft(List(1, 2, 3, 4), 1)(_ * _) shouldBe 24
    List.foldLeft(List(1, 2, 3, 4), 0)((_, b) => b + 1) shouldBe 4
  }

  // exercise 3.11
  it should "calculate the sum of a list of ints using foldLeft" in {
    List.sum3(List(1, 2, 3, 4)) shouldBe 10
  }

  it should "calculate the product of a list of doubles using foldLeft" in {
    List.product3(List(1, 2, 3, 4)) shouldBe 24
  }

  it should "calculate the length of a list using foldLeft" in {
    List.length3(List(1, 2, 3, 4)) shouldBe 4
  }

  // exercise 3.12
  it should "calculate the reverse of a list using foldLeft" in {
    List.reverse(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)
    List.reverse(Nil) shouldBe Nil
    List.reverse(List(1)) shouldBe List(1)
  }

  it should "express foldLeft in terms of foldRight" in {
    List.foldLeftViaFoldRight(List(1, 2, 3, 4), 0)(_ + _) shouldBe
      List.foldLeft(List(1, 2, 3, 4), 0)(_ + _)
    List.foldLeftViaFoldRight(List(1, 2, 3, 4), 0)(_ * _) shouldBe
      List.foldLeft(List(1, 2, 3, 4), 0)(_ * _)
    List.foldLeftViaFoldRight(List("a", "b", "c", "d"), "")(_ + _) shouldBe
      List.foldLeft(List("a", "b", "c", "d"), "")(_ + _)
  }

  it should "express foldRight in terms of foldLeft" in {
    List.foldRightViaFoldLeft(List(1, 2, 3, 4), 0)(_ + _) shouldBe
      List.foldRight(List(1, 2, 3, 4), 0)(_ + _)
    List.foldRightViaFoldLeft(List(1, 2, 3, 4), 0)(_ * _) shouldBe
      List.foldRight(List(1, 2, 3, 4), 0)(_ * _)
    List.foldRightViaFoldLeft(List("a", "b", "c", "d"), "")(_ + _) shouldBe
      List.foldRight(List("a", "b", "c", "d"), "")(_ + _)
  }
}
