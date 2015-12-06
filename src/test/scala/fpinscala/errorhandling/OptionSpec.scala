package fpinscala.errorhandling

import fpinscala.datastructures
import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {
  // exercise 4.1
  "An Option" should "map its value from one type to another" in {
    Some(3).map(_.toString) shouldBe Some("3")
    None.map(_.toString) shouldBe None
  }

  it should "flatmap its value from one type to another" in {
    Some(3).flatMap(x => Some(x.toString)) shouldBe Some("3")
    None.flatMap(x => Some(x.toString)) shouldBe None
  }

  it should "return its value if present or a default" in {
    Some(3).getOrElse(2) shouldBe 3
    None.getOrElse(2) shouldBe 2
  }

  it should "return itself if present or a default" in {
    Some(3).orElse(Some(2)) shouldBe Some(3)
    None.orElse(Some(2)) shouldBe Some(2)
  }

  it should "filter its value based on a predicate" in {
    Some(3).filter(_ > 2) shouldBe Some(3)
    Some(3).filter(_ < 2) shouldBe None
    (None: Option[Int]).filter(_ < 2) shouldBe None
  }

  "An Option companion object" should "lift a function" in {
    Option.lift((a: Int) => a * 2)(Some(3)) shouldBe Some(6)
    Option.lift((a: Int) => a * 2)(None) shouldBe None
  }

  // exercise 4.3
  it should "combine two options using a binary function" in {
    Option.map2(Some(3), Some(4))(_ * _) shouldBe Some(12)
    Option.map2(Some(3), None: Option[Int])(_ * _) shouldBe None
    Option.map2(Some(12.0), Some(3))((a: Double, b: Int) => (a/b).toString)
  }

  // exercise 4.4
  it should "sequence a list of options into a single option of a list of the some values" in {
    Option.sequence(List(Some(3), Some(4), None, Some(5), None)) shouldBe Some(List(3,4,5))
  }
}
