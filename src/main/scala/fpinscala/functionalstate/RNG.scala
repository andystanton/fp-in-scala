package fpinscala.functionalstate

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
}

trait RNG {
  import fpinscala.functionalstate.RNG.Rand

  def nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt
}

