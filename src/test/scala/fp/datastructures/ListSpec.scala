package fp.datastructures

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
    List.sumViaFoldRight(List(1, 2, 3, 4)) shouldBe 10
  }

  it should "calculate the product of a list of double values" in {
    List.product(List(1.0, 2.0, 3.0, 4.0)) shouldBe 24.0
  }

  it should "calculate the product of a list of double values using foldRight" in {
    List.productViaFoldRight(List(1.0, 2.0, 3.0, 4.0)) shouldBe 24.0
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
    List.lengthViaFoldRight(List(1, 2, 3, 4)) shouldBe 4
    List.lengthViaFoldRight(List(1)) shouldBe 1
    List.lengthViaFoldRight(Nil) shouldBe 0
  }

  // exercise 3.10
  it should "foldLeft over a list" in {
    List.foldLeft(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
    List.foldLeft(List(1, 2, 3, 4), 1)(_ * _) shouldBe 24
    List.foldLeft(List(1, 2, 3, 4), 0)((b, _) => b + 1) shouldBe 4
  }

  // exercise 3.11
  it should "calculate the sum of a list of ints using foldLeft" in {
    List.sumViaFoldLeft(List(1, 2, 3, 4)) shouldBe 10
  }

  it should "calculate the product of a list of doubles using foldLeft" in {
    List.productViaFoldLeft(List(1, 2, 3, 4)) shouldBe 24
  }

  it should "calculate the length of a list using foldLeft" in {
    List.lengthViaFoldLeft(List(1, 2, 3, 4)) shouldBe 4
  }

  // exercise 3.12
  it should "calculate the reverse of a list using foldLeft" in {
    List.reverse(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)
    List.reverse(Nil) shouldBe Nil
    List.reverse(List(1)) shouldBe List(1)
  }

  // exercise 3.13
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

  // exercise 3.14
  it should "append two lists together via foldRight" in {
    List.appendViaFoldRight(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
    List.appendViaFoldRight(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
    List.appendViaFoldRight(Nil, List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  it should "append two lists together via foldLeft" in {
    List.appendViaFoldLeft(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
    List.appendViaFoldLeft(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
    List.appendViaFoldLeft(Nil, List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  // exercise 3.15
  it should "concatenate several lists together" in {
    List.concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) shouldBe
      List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  // exercise 3.16
  it should "add one to a list of integers" in {
    List.addOne(List(1, 2, 3, 4)) shouldBe List(2, 3, 4, 5)
  }

  // exercise 3.17
  it should "turn a list of doubles into a list of strings" in {
    List.doubleToString(List(1.1, 2.2, 3.3, 4.4)) shouldBe
      List("1.1", "2.2", "3.3", "4.4")
  }

  // exercise 3.18
  it should "convert a list from one type into another" in {
    List.map(List(1, 2, 3, 4))(_ + 1) shouldBe List(2, 3, 4, 5)
    List.map(List(1.1, 2.2, 3.3, 4.4))(_.toString) shouldBe
      List("1.1", "2.2", "3.3", "4.4")
  }

  // exercise 3.19
  it should "filter a list of elements that satisfy a given predicate" in {
    List.filter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_ % 2 == 1) shouldBe List(2, 4, 6, 8, 10)
  }

  // exercise 3.20
  it should "convert a list from one type into a list of another type" in {
    List.flatMap(List(1, 2, 3, 4))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3, 4, 4)
  }

  // exercise 3.21
  it should "filter a list of elements that satisfy a given predicate in terms of flatMap" in {
    List.filterViaFlatMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_ % 2 == 1) shouldBe List(2, 4, 6, 8, 10)
  }

  // exercise 3.22
  it should "add two lists of ints together" in {
    List.add(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }

  // exercise 3.23
  it should "zip two lists together" in {
    List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
    List.zipWith(List(1, 2, 3), List(4, 5, 6))((_, _)) shouldBe List((1, 4), (2, 5), (3, 6))
  }

  // exercise 3.24
  it should "find subsequences of a list" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(1)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(2)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(3)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(3, 4)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(4)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 1)) shouldBe false
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4, 5)) shouldBe false
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 3, 4)) shouldBe false
  }
}
