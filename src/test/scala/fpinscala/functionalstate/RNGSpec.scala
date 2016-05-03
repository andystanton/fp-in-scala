package fpinscala.functionalstate

import org.scalacheck.Gen
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

  // exercise 6.3
  "intDouble" should "generate an int/double pair" in {
    forAll { seed: Long =>
      val res = SimpleRNG.intDouble(SimpleRNG(seed))._1
      res._1 should be <= Int.MaxValue
      res._1 should be >= 0
      res._2 should be >= 0.0
      res._2 should be <= 1.0
    }
  }

  "doubleInt" should "generate a double/int pair" in {
    forAll { seed: Long =>
      val res = SimpleRNG.doubleInt(SimpleRNG(seed))._1
      res._1 should be >= 0.0
      res._1 should be <= 1.0
      res._2 should be <= Int.MaxValue
      res._2 should be >= 0
    }
  }

  "double3" should "generate a double 3-tuple" in {
    forAll { seed: Long =>
      val res = SimpleRNG.double3(SimpleRNG(seed))._1
      res._1 should be >= 0.0
      res._1 should be <= 1.0
      res._2 should be >= 0.0
      res._2 should be <= 1.0
      res._3 should be >= 0.0
      res._3 should be <= 1.0
    }
  }

  // exercise 6.4
  "ints" should "generate a list of random ints" in {
    forAll(Gen.chooseNum(Long.MinValue, Long.MaxValue), Gen.posNum[Int]) { (seed: Long, count: Int) =>
      val res = SimpleRNG.ints(count)(SimpleRNG(seed))
      res._1.length shouldBe count
      res._1.foreach(randomInt => {
        randomInt should be >= 0
        randomInt should be <= Int.MaxValue
      })
    }
  }

  // exercise 6.5
  "doubleViaMap" should "generate double between 0 and 1" in {
    forAll { seed: Long =>
      val res = SimpleRNG.doubleViaMap(SimpleRNG(seed))
      res._1 should be >= 0.0
      res._1 should be <= 1.0
    }
  }
}
