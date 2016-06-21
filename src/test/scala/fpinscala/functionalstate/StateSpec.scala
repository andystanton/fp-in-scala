package fpinscala.functionalstate

import fpinscala.functionalstate.State.Rand.int
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "State unit" should "return the unit value" in {
    forAll { seed: Long =>
      val (res, _) = State.unit(3.0).run(SimpleRNG(seed))
      res shouldBe 3.0
    }
  }

  "State map" should "map" in {
    forAll { seed: Long =>
      val (res, _) = int.map(_.toDouble).run(SimpleRNG(seed))
      res should be >= Double.MinValue
      res should be <= Double.MaxValue
    }
  }
}
