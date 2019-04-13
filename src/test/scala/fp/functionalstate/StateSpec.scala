package fp.functionalstate

import fp.functionalstate.State.Rand
import fp.functionalstate.State.Rand.{int, double}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "unit" should "return the unit state" in {
    forAll { seed: Long =>
      val (res, _) = State.unit(3.0).run(SimpleRNG(seed))
      res shouldBe 3.0
    }
  }

  "map" should "map" in {
    forAll { seed: Long =>
      val (res, _) = int.map(_.toDouble).run(SimpleRNG(seed))
      res should be >= Double.MinValue
      res should be <= Double.MaxValue
    }
  }

  "flatMap" should "flatMap" in {
    forAll { seed: Long =>
      val (res, _) = int.flatMap(Rand.toDouble).run(SimpleRNG(seed))
      res shouldBe a[Double]
      res should be >= 0.0
      res should be <= 1.0
    }
  }

  "map2" should "combine two states" in {
    forAll { seed: Long =>
      val i1: Rand[Int] = int
      val i2: Rand[Int] = int

      val rng: SimpleRNG = SimpleRNG(seed)
      val (res, _) = State.map2(i1, i2)(_ + _).run(rng)

      res should be <= Int.MaxValue
      res should be >= Int.MinValue

      val i1Computed = i1.run(rng)
      val i2Computed = i2.run(i1Computed._2)

      res shouldBe i1Computed._1 + i2Computed._1
    }
  }

  "sequence" should "sequence a list of states into a state over a list" in {
    forAll { seed: Long =>
      val inList = List(State.unit[RNG, Int](3), State.unit[RNG, Double](8435.21), int, double)
      val (res, _) = State.sequence[RNG, Any](inList).run(SimpleRNG(seed))

      res.length shouldBe inList.length

      val ui :: ud :: i :: d :: Nil = res

      ui shouldBe an[Int]
      ui shouldBe 3
      ud shouldBe a[Double]
      ud shouldBe 8435.21
      i shouldBe an[Int]
      d shouldBe a[Double]
    }
  }

  "get" should "return the current state" in {
    forAll { seed: Long =>
      val rng: SimpleRNG = SimpleRNG(seed)
      val (r1, r2) = State.get.run(rng)
      r1 shouldBe rng
      r1 shouldBe r2
    }
  }

  "set" should "update the current state" in {
    forAll { seed: Long =>
      val rng: SimpleRNG = SimpleRNG(seed)
      val rng2: SimpleRNG = SimpleRNG(seed)
      val (r1, r2) = State.set(rng2).run(rng)
      r1 shouldBe (())
      r2 shouldBe rng
    }
  }

  "modify" should "modify the current state" in {
    forAll { seed: Long =>
      val rng: SimpleRNG = SimpleRNG(seed)
      val rng2: SimpleRNG = SimpleRNG(seed)
      val (r1, r2) = State.modify[RNG](_ => rng2).run(rng)
      r1 shouldBe (())
      r2 shouldBe rng2
    }
  }
}
