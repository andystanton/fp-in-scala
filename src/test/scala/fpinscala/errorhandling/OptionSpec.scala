package fpinscala.errorhandling

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {
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
}
