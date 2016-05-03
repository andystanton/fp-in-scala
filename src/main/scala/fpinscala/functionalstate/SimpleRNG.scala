package fpinscala.functionalstate

import fpinscala.functionalstate.RNG.Rand

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {
  // exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (num, nextRNG) if num == Int.MinValue => nonNegativeInt(nextRNG)
    case (num, nextRNG) => (Math.abs(num), nextRNG)
  }

  // exercise 6.2
  def double(rng: RNG) = {
    val nni = nonNegativeInt(rng)
    (nni._1.toDouble / Int.MaxValue, nni._2)
  }

  // exercise 6.3
  def intDouble(rng: SimpleRNG) = {
    val i = nonNegativeInt(rng)
    val d = double(i._2)
    ((i._1, d._1), d._2)
  }

  def doubleInt(rng: SimpleRNG) = {
    val d = double(rng)
    val i = nonNegativeInt(d._2)
    ((d._1, i._1), i._2)
  }

  def double3(rng: SimpleRNG) = {
    val d1 = double(rng)
    val d2 = double(d1._2)
    val d3 = double(d2._2)
    ((d1._1, d2._1, d3._1), d3._2)
  }

  // exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, rng: RNG, build: List[Int]): (List[Int], RNG) = {
      count match {
        case 0 => (build, rng)
        case _ =>
          val i = nonNegativeInt(rng)
          loop(count - 1, i._2, i._1 :: build)
      }
    }
    loop(count, rng, Nil)
  }

  // exercise 6.5
  def doubleViaMap: Rand[Double] = rng =>
    RNG.map(rng.int)(i => Math.abs(i.toDouble) / Int.MaxValue)(rng)
}
