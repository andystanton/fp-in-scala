package fpinscala.errorhandling

import org.scalatest._

class VarianceSpec extends FlatSpec with Matchers {
  // exercise 4.2
  "The variance function" should "calculate the variance of a sequence" in {
    Variance.variance(Range.Double.inclusive(1.0, 10.0, 1.0)) shouldBe Some(8.25)
    Variance.variance(Seq()) shouldBe None
  }
}
