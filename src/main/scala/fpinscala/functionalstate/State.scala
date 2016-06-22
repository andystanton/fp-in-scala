package fpinscala.functionalstate

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State[S, B](stateA => {
    val (output, stateB) = run(stateA)
    (f(output), stateB)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B](stateA => {
    val (a, stateB) = run(stateA)
    f(a).run(stateB)
  })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(stateA => (a, stateA))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = State(stateA => {
    val (a, stateB) = ra.run(stateA)
    val (b, stateC) = rb.run(stateB)
    (f(a, b), stateC)
  })

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil: List[A]))((ra, carried) => map2(ra, carried)(_ :: _))

  type Rand[A] = State[RNG, A]

  object Rand {
    def int = new Rand(_.nextInt)
    def double = new Rand(_.nextInt).map(_.toDouble)
    def toDouble(a: Int) = unit[RNG, Int](a).map(i => Math.abs(i.toDouble) / Integer.MAX_VALUE)
  }
}