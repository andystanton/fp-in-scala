package fpinscala.errorhandling

import fpinscala.datastructures._
import org.scalatest._

class EitherSpec extends FlatSpec with Matchers {
  // exercise 4.6
  "An Either" should "map its value" in {
    val first: Either[String, Int] = Left("no")
    val second: Either[String, Int] = Right(3)
    first.map(_.toString) shouldBe Left("no")
    second.map(_.toString) shouldBe Right("3")
  }

  it should "return a sensible default" in {
    Right(3).orElse(Right(2)) shouldBe Right(3)
    Left("no").orElse(Right(2)) shouldBe Right(2)
  }

  it should "flatMap its value" in {
    val first: Either[String, Int] = Left("no")
    val second: Either[String, Int] = Right(3)
    first.flatMap(x => Left(x.toString)) shouldBe Left("no")
    second.flatMap(x => Right(x.toString)) shouldBe Right("3")
  }

  it should "map apply a function to itself and another Either" in {
    val first: Either[String, Int] = Right(4)
    val second: Either[String, Int] = Right(3)
    first.map2(second)(_ * _) shouldBe Right(12)

    val third: Either[String, Int] = Left("no")
    val fourth: Either[String, Int] = Right(3)
    third.map2(fourth)(_ * _) shouldBe Left("no")
  }

  // exercise 4.7
  it should "sequence a List of Eithers into an Either of a List" in {
    val first: Either[String, Int] = Right(4)
    val second: Either[String, Int] = Right(3)
    val third: Either[String, Int] = Left("no")
    val fourth: Either[String, Int] = Right(2)
    val fifth: Either[String, Int] = Left("not at all")

    Either.sequence(List(first, second, fourth)) shouldBe Right(List(4, 3, 2))
    Either.sequence(List(first, second, third, fourth, fifth)) shouldBe Left("no")
  }

  it should "traverse a List of Eithers calling a function on each and returning an Either of a list" in {
    Either.traverse(List(4, 3, 2, 1))(x => Right(x.toString)) shouldBe Right(List("4", "3", "2", "1"))
    Either.traverse(List(4, 3, 2, 1))({
      case 3 => Left("no")
      case x => Right(x.toString)
    }) shouldBe Left("no")
  }
}
