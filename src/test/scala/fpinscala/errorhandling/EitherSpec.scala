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
    first.map2(second)(_*_) shouldBe Right(12)

    val third: Either[String, Int] = Left("no")
    val fourth: Either[String, Int] = Right(3)
    third.map2(fourth)(_*_) shouldBe Left("no")
  }
}
