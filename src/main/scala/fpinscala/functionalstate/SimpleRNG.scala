package fpinscala.functionalstate

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
  def double(rng: SimpleRNG) = {
    val nni = nonNegativeInt(rng)
    (nni._1.toDouble / Int.MaxValue, nni._2)
  }
}
