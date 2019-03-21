package fp.functionalstate

import fp.functionalstate.RNG._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RNGSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A Random Number Generator" should "generate integers between min and max inclusive" in {
    forAll { seed: Long =>
      val (res, _) = SimpleRNG(seed).nextInt
      res should be >= Int.MinValue
      res should be <= Int.MaxValue
    }
  }

  // exercise 6.1
  "nonNegativeInt" should "generate integers between 0 and max inclusive" in {
    forAll { seed: Long =>
      val (res, _) = nonNegativeInt(SimpleRNG(seed))
      res should be >= 0
      res should be <= Int.MaxValue
    }
  }

  // exercise 6.2
  "double" should "generate double between 0 and 1" in {
    forAll { seed: Long =>
      val (res, _) = double(SimpleRNG(seed))
      res should be >= 0.0
      res should be <= 1.0
    }
  }

  // exercise 6.3
  "intDouble" should "generate an int/double pair" in {
    forAll { seed: Long =>
      val ((int, double), _) = intDouble(SimpleRNG(seed))
      int should be <= Int.MaxValue
      int should be >= 0
      double should be >= 0.0
      double should be <= 1.0
    }
  }

  "doubleInt" should "generate a double/int pair" in {
    forAll { seed: Long =>
      val ((d, i), _) = doubleInt(SimpleRNG(seed))
      d should be >= 0.0
      d should be <= 1.0
      i should be <= Int.MaxValue
      i should be >= 0
    }
  }

  "double3" should "generate a double 3-tuple" in {
    forAll { seed: Long =>
      val ((d1, d2, d3), _) = double3(SimpleRNG(seed))
      d1 should be >= 0.0
      d1 should be <= 1.0
      d2 should be >= 0.0
      d2 should be <= 1.0
      d3 should be >= 0.0
      d3 should be <= 1.0
    }
  }

  // exercise 6.4
  "ints" should "generate a list of random ints" in {
    forAll(Gen.chooseNum(Long.MinValue, Long.MaxValue), Gen.posNum[Int]) { (seed: Long, count: Int) =>
      val (res, _) = ints(count)(SimpleRNG(seed))
      res.length shouldBe count
      res.foreach(randomInt => {
        randomInt should be >= 0
        randomInt should be <= Int.MaxValue
      })
    }
  }

  // exercise 6.5
  "map" should "map" in {
    forAll { seed: Long =>
      val (res, _) = map(int)(_.toDouble)(SimpleRNG(seed))
      res should be >= Double.MinValue
      res should be <= Double.MaxValue
    }
  }

  "nonNegativeEvensViaMap" should "generate non negative events" in {
    forAll { seed: Long =>
      val (res, _) = nonNegativeEvenViaMap(SimpleRNG(seed))
      res % 2 shouldBe 0
      res should be >= 0
    }
  }

  "doubleViaMap" should "generate double between 0 and 1" in {
    forAll { seed: Long =>
      val (res, _) = doubleViaMap(SimpleRNG(seed))
      res should be >= 0.0
      res should be <= 1.0
    }
  }

  // exercise 6.6
  "map2" should "combine two Rand expressions" in {
    forAll { seed: Long =>
      val i1: Rand[Int] = int
      val i2: Rand[Int] = int

      val (res, _) = map2(i1, i2)(_ + _)(SimpleRNG(seed))

      res should be <= Int.MaxValue
      res should be >= Int.MinValue
    }
  }

  "both" should "return the results of two Rand expressions" in {
    forAll { seed: Long =>
      val i1: Rand[Int] = int
      val i2: Rand[Int] = int

      val ((int1, int2), _) = both(i1, i2)(SimpleRNG(seed))

      int1 should be <= Int.MaxValue
      int1 should be >= Int.MinValue

      int2 should be <= Int.MaxValue
      int2 should be >= Int.MinValue
    }
  }

  "randIntDouble" should "generate an int/double pair" in {
    forAll { seed: Long =>
      val ((int, double), _) = randIntDouble(SimpleRNG(seed))
      int should be <= Int.MaxValue
      int should be >= 0
      double should be >= 0.0
      double should be <= 1.0
    }
  }

  "randDoubleInt" should "generate a double/int pair" in {
    forAll { seed: Long =>
      val ((double, int), _) = randDoubleInt(SimpleRNG(seed))
      double should be >= 0.0
      double should be <= 1.0
      int should be <= Int.MaxValue
      int should be >= 0
    }
  }

  // exercise 6.7
  "sequence" should "sequence a list of rands into a rand over a list" in {
    forAll { seed: Long =>
      val inList = List(randIntDouble, randDoubleInt, int, doubleViaMap)
      val (res, _) = sequence(inList)(SimpleRNG(seed))

      res.length shouldBe inList.length

      val id :: di :: i :: d :: Nil = res

      id shouldBe a[(_, _)]
      di shouldBe a[(_, _)]
      i shouldBe a[java.lang.Integer]
      d shouldBe a[java.lang.Double]
    }
  }

  // exercise 6.8
  "flatMap" should "flatMap" in {
    forAll { seed: Long =>
      val (res, _) = flatMap(int)(a => doubleViaMap)(SimpleRNG(seed))
      res shouldBe a[java.lang.Double]
      res should be >= 0.0
      res should be <= 1.0
    }
  }

  "nonNegativeLessThan" should "generate non negative ints less than n" in {
    forAll { seed: Long =>
      val (res, _) = nonNegativeLessThan(10)(SimpleRNG(seed))
      res should be >= 0
      res should be < 10
    }
  }

  // exercise 6.9
  "mapViaFlatMap" should "map" in {
    forAll { seed: Long =>
      val (res, _) = mapViaFlatMap(int)(_.toDouble)(SimpleRNG(seed))
      res should be >= Double.MinValue
      res should be <= Double.MaxValue
    }
  }

  "map2ViaFlatMap" should "combine two Rand expressions" in {
    forAll { seed: Long =>
      val i1: Rand[Int] = int
      val i2: Rand[Int] = int

      val (res, _) = map2ViaFlatMap(i1, i2)(_ + _)(SimpleRNG(seed))

      res should be <= Int.MaxValue
      res should be >= Int.MinValue
    }
  }
}
