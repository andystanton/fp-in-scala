package fpinscala.functionalstate

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  type MachineState = State[Machine, (Int, Int)]

  // exercise 6.11
  def simulateMachine(inputs: List[Input]): MachineState = inputs match {
    case Nil => throw new IllegalArgumentException("Cannot simulate machine with no inputs")
    case _ => State.sequence(inputs.map {
      case Coin => new MachineState({
        case Machine(true, candies, coins) if candies > 0 =>
          ((coins + 1, candies), Machine(locked = false, candies, coins + 1))
        case machine =>
          ((machine.coins, machine.candies), machine)
      })
      case Turn => new MachineState({
        case machine@Machine(true, candies, coins) =>
          ((coins, candies), machine)
        case Machine(_, candies, coins) =>
          ((coins, candies - 1), Machine(locked = true, candies - 1, coins))
      })
    }).map(_.last)
  }
}