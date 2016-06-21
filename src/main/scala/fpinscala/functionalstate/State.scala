package fpinscala.functionalstate

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State[S, B](stateIn => {
    val (output, stateOut) = run(stateIn)
    (f(output), stateOut)
  })
}

object State {

  def unit[S, A](a: A): State[S, A] = State(stateIn => (a, stateIn))

  type Rand[A] = State[RNG, A]

  object Rand {
    def int = new Rand(_.nextInt)
  }
}