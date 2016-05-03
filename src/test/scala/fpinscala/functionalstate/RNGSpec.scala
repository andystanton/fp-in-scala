package fpinscala.functionalstate

import fpinscala.functionalstate.RNG.Rand
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
      val res = RNG.nonNegativeInt(SimpleRNG(seed))
      res._1 should be >= 0
      res._1 should be <= Int.MaxValue
    }
  }

  // exercise 6.2
  "double" should "generate double between 0 and 1" in {
    forAll { seed: Long =>
      val res = RNG.double(SimpleRNG(seed))
      res._1 should be >= 0.0
      res._1 should be <= 1.0
    }
  }

  // exercise 6.3
  "intDouble" should "generate an int/double pair" in {
    forAll { seed: Long =>
      val res = RNG.intDouble(SimpleRNG(seed))._1
      res._1 should be <= Int.MaxValue
      res._1 should be >= 0
      res._2 should be >= 0.0
      res._2 should be <= 1.0
    }
  }

  "doubleInt" should "generate a double/int pair" in {
    forAll { seed: Long =>
      val res = RNG.doubleInt(SimpleRNG(seed))._1
      res._1 should be >= 0.0
      res._1 should be <= 1.0
      res._2 should be <= Int.MaxValue
      res._2 should be >= 0
    }
  }

  "double3" should "generate a double 3-tuple" in {
    forAll { seed: Long =>
      val res = RNG.double3(SimpleRNG(seed))._1
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
      val res = RNG.ints(count)(SimpleRNG(seed))._1
      res.length shouldBe count
      res.foreach(randomInt => {
        randomInt should be >= 0
        randomInt should be <= Int.MaxValue
      })
    }
  }

  // exercise 6.5
  "nonNegativeEvensViaMap" should "generate non negative events" in {
    forAll { seed: Long =>
      val res = RNG.nonNegativeEvenViaMap(SimpleRNG(seed))._1
      res % 2 shouldBe 0
      res should be >= 0
    }
  }

  "doubleViaMap" should "generate double between 0 and 1" in {
    forAll { seed: Long =>
      val res = RNG.doubleViaMap(SimpleRNG(seed))._1
      res should be >= 0.0
      res should be <= 1.0
    }
  }

  // exercise 6.6
  "map2" should "combine two Rand expressions" in {
    forAll { seed: Long =>
      val i1: Rand[Int] = RNG.int
      val i2: Rand[Int] = RNG.int

      val res = RNG.map2(i1, i2)(_ + _)(SimpleRNG(seed))._1

      res should be <= Int.MaxValue
      res should be >= Int.MinValue
    }
  }

  "both" should "return the results of two Rand expressions" in {
    forAll { seed: Long =>
      val i1: Rand[Int] = RNG.int
      val i2: Rand[Int] = RNG.int

      val res = RNG.both(i1, i2)(SimpleRNG(seed))._1

      res._1 should be <= Int.MaxValue
      res._1 should be >= Int.MinValue

      res._2 should be <= Int.MaxValue
      res._2 should be >= Int.MinValue
    }
  }

  "randIntDouble" should "generate an int/double pair" in {
    forAll { seed: Long =>
      val res = RNG.randIntDouble(SimpleRNG(seed))._1
      res._1 should be <= Int.MaxValue
      res._1 should be >= 0
      res._2 should be >= 0.0
      res._2 should be <= 1.0
    }
  }

  "randDoubleInt" should "generate a double/int pair" in {
    forAll { seed: Long =>
      val res = RNG.randDoubleInt(SimpleRNG(seed))._1
      res._1 should be >= 0.0
      res._1 should be <= 1.0
      res._2 should be <= Int.MaxValue
      res._2 should be >= 0
    }
  }

  // exercise 6.7
  "sequence" should "sequence a list of rands into a rand over a list" in {
    forAll { seed: Long =>
      val inList = List(RNG.randIntDouble, RNG.randDoubleInt, RNG.int, RNG.doubleViaMap)
      val res = RNG.sequence(inList)(SimpleRNG(seed))._1

      res.length shouldBe inList.length

      res(0) shouldBe a[(_, _)]
      res(1) shouldBe a[(_, _)]
      res(2) shouldBe a[java.lang.Integer]
      res(3) shouldBe a[java.lang.Double]
    }
  }
}
