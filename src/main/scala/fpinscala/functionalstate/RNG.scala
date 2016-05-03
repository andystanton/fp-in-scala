package fpinscala.functionalstate

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

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
  val int: Rand[Int] = _.nextInt

  def doubleViaMap: Rand[Double] = map(int)(i => Math.abs(i.toDouble) / Int.MaxValue)

  def nonNegativeIntViaMap: Rand[Int] = map(int)(Math.abs)

  def nonNegativeEvenViaMap: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  // exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDouble = both(nonNegativeIntViaMap, doubleViaMap)

  def randDoubleInt = both(doubleViaMap, nonNegativeIntViaMap)

  // exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((ra, carried) => map2(ra, carried)(_ :: _))

  // exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2): (A, RNG) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeIntViaMap) { i =>
    val mod = i % n
    if ((i + n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  // exercise 6.9
  def mapViaFlatMap[A, B](a: Rand[A])(f: A => B): Rand[B] = flatMap(a)(ia => unit(f(ia)))

  def map2ViaFlatMap[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(a)(ia => flatMap(b)(ib => unit(f(ia, ib))))
}
