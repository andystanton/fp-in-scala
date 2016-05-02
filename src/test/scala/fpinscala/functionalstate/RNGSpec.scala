package fpinscala.functionalstate

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RNGSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A Random Number Generator" should "generate integers between min and max inclusive" in {
    forAll { seed: Long =>
      val res = SimpleRNG(seed).nextInt
      res._1 should be >= Int.MinValue
      res._1 should be <= Int.MaxValue
    }
  }

  // exercise 6.1
  "nonNegativeInt" should "generate integers between 0 and max inclusive" in {
    forAll { seed: Long =>
      val res = SimpleRNG.nonNegativeInt(SimpleRNG(seed))
      res._1 should be >= 0
      res._1 should be <= Int.MaxValue
    }
  }

  // exercise 6.2
  "double" should "generate double between 0 and 1" in {
    forAll { seed: Long =>
      val res = SimpleRNG.double(SimpleRNG(seed))
      res._1 should be >= 0.0
      res._1 should be <= 1.0
    }
  }
}
