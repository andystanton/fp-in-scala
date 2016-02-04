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

    // must be same length even with same beginning
    Stream(1, 2, 3, 4) === Stream(1, 2, 3, 4, 5) shouldBe false
    Stream(1, 2, 3, 4, 5) === Stream(1, 2, 3, 4) shouldBe false

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

  // exercise 5.3
  it should "take the first n elements matching a predicate" in {
    Stream(1, 2, 3, 4, 5).takeWhile(_ < 4) === Stream(1, 2, 3) shouldBe true
  }

  it should "fold right over a stream" in {
    Stream(1, 2, 3, 4, 5).foldRight(0)(_ + _) shouldBe 15
  }

  it should "fold left over a stream" in {
    Stream(1, 2, 3, 4, 5).foldLeft(0)(_ + _) shouldBe 15
  }

  // exercise 5.4
  it should "assert a predicate over its elements via foldRight" in {
    Stream(1, 2, 3, 4, 5).forAllViaFoldRight(_ < 10) shouldBe true
    Stream(1, 2, 3, 4, 5).forAllViaFoldRight(_ < 3) shouldBe false
  }

  it should "assert a predicate over its elements" in {
    Stream(1, 2, 3, 4, 5).forAll(_ < 10) shouldBe true
    Stream(1, 2, 3, 4, 5).forAll(_ < 3) shouldBe false
  }

  // exercise 5.5
  it should "take the first n elements matching a predicate via foldRight" in {
    Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ < 4) === Stream(1, 2, 3) shouldBe true
  }

  // exercise 5.6
  it should "return an Option of the first item in the stream via foldRight" in {
    Stream(1, 2, 3, 4, 5).headOptionViaFoldRight shouldBe Some(1)
    Stream.empty.headOptionViaFoldRight shouldBe None
  }

  // exercise 5.7
  it should "map over its elements" in {
    Stream(1, 2, 3, 4, 5).map(_ * 2) === Stream(2, 4, 6, 8, 10) shouldBe true
  }

  it should "filter its elements" in {
    Stream(1, 2, 3, 4, 5).filter(_ % 2 == 1) === Stream(1, 3, 5) shouldBe true
    Stream.empty[Int].filter(_ % 2 == 1) === Stream.empty shouldBe true
  }

  it should "append another Stream" in {
    Stream(1, 2, 3, 4, 5).append(Stream(6, 7, 8, 9, 10)) === Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) shouldBe true
    Stream.empty[Int].append(Stream(8)) === Stream(8) shouldBe true
  }

  it should "flatMap over its elements" in {
    Stream(1, 2, 3, 4, 5).flatMap(i => Stream(i, i)) === Stream(1, 1, 2, 2, 3, 3, 4, 4, 5, 5) shouldBe true
  }

  it should "find an element" in {
    Stream(1, 2, 3, 4, 5).find(_ == 3) shouldBe Some(3)
    Stream(1, 2, 3, 4, 5).find(_ == 10) shouldBe None
  }

  it should "return an infinite stream of ones" in {
    Stream.ones.take(5) === Stream(1, 1, 1, 1, 1) shouldBe true
  }

  // exercise 5.8
  it should "return an infinite stream of a constant" in {
    Stream.constant(1).take(5) === Stream(1, 1, 1, 1, 1) shouldBe true
  }

  // exercise 5.9
  it should "generate an infinite of integers" in {
    Stream.from(4).take(5) === Stream(4, 5, 6, 7, 8) shouldBe true
  }

  // exercise 5.10
  it should "generate the Fibonacci sequence" in {
    Stream.fibs.take(10) === Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34) shouldBe true
  }

  // exercise 5.11
  it should "construct Streams in a general way" in {
    Stream.unfold(3)(carried =>
      Some((carried, carried + 2))).take(5) === Stream(3, 5, 7, 9, 11) shouldBe true

    Stream.unfold(3)(carried =>
      if (carried > 5) None else Some((carried, carried + 1))).take(5) === Stream(3, 4, 5) shouldBe true
  }

  // exercise 5.12
  it should "return an infinite stream of ones via unfold" in {
    Stream.onesViaUnfold.take(5) === Stream(1, 1, 1, 1, 1) shouldBe true
  }

  it should "return an infinite stream of a constant via unfold" in {
    Stream.constantViaUnfold(1).take(5) === Stream(1, 1, 1, 1, 1) shouldBe true
  }

  it should "generate an infinite of integers via unfold" in {
    Stream.fromViaUnfold(4).take(5) === Stream(4, 5, 6, 7, 8) shouldBe true
  }

  it should "generate the Fibonacci sequence via unfold" in {
    Stream.fibsViaUnfold.take(10) === Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34) shouldBe true
  }

  // exercise 5.13
  it should "map over its elements via unfold" in {
    Stream(1, 2, 3, 4, 5).mapViaUnfold(_ * 2) === Stream(2, 4, 6, 8, 10) shouldBe true
  }

  it should "take the first n elements via unfold" in {
    Stream(1, 2, 3, 4, 5).takeViaUnfold(3) === Stream(1, 2, 3) shouldBe true
  }

  it should "take the first n elements matching a predicate via unfold" in {
    Stream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ < 4) === Stream(1, 2, 3) shouldBe true
  }

  it should "zip two streams together via unfold" in {
    Stream(1, 2, 3).zipWithViaUnfold(Stream(4, 5, 6))(_ + _) ===
      Stream(5, 7, 9) shouldBe true
    Stream(1, 2, 3).zipWithViaUnfold(Stream(4, 5, 6))((_, _)) ===
      Stream((1, 4), (2, 5), (3, 6)) shouldBe true
    Stream(1, 2, 3, 4, 5, 6).zipWithViaUnfold(Stream(4, 5, 6))(_ + _) ===
      Stream(5, 7, 9) shouldBe true
  }

  it should "zip two streams together until one of them ends" in {
    Stream(1, 2, 3).zipAll(Stream(4, 5, 6, 7, 8, 9)) === Stream(
      (Some(1), Some(4)),
      (Some(2), Some(5)),
      (Some(3), Some(6)),
      (None, Some(7)),
      (None, Some(8)),
      (None, Some(9))) shouldBe true

    Stream(1, 2, 3, 7, 8, 9).zipAll(Stream(4, 5, 6)) === Stream(
      (Some(1), Some(4)),
      (Some(2), Some(5)),
      (Some(3), Some(6)),
      (Some(7), None),
      (Some(8), None),
      (Some(9), None)) shouldBe true
  }
}
