package fpinscala.functionalstate

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RNGSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A Random Number Generator" should "generate longs between min and max" in {
    forAll { seed: Long =>
      val res = SimpleRNG(seed).nextInt
      res._1 should be >= Int.MinValue
      res._1 should be <= Int.MaxValue
    }
  }
}
