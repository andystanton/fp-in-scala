package fp.functionalstate
import scala.collection.immutable

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  type MachineState = State[Machine, (Int, Int)]

  // exercise 6.11
//  def simulateMachine(inputs: List[Input]): MachineState = inputs match {
//    case Nil => throw new IllegalArgumentException("Cannot simulate machine with no inputs")
//    case _ => State.sequence(inputs.map {
//      case Coin => new MachineState({
//        case Machine(true, candies, coins) if candies > 0 =>
//          ((coins + 1, candies), Machine(locked = false, candies, coins + 1))
//        case machine =>
//          ((machine.coins, machine.candies), machine)
//      })
//      case Turn => new MachineState({
//        case machine@Machine(true, candies, coins) =>
//          ((coins, candies), machine)
//        case Machine(_, candies, coins) =>
//          ((coins, candies - 1), Machine(locked = true, candies - 1, coins))
//      })
//    }).map(_.last)
//  }

  private def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    t <- {
      val potato: State[Machine, List[Unit]] = State.sequence(inputs.map((input: Input) => {
        val function: Input => State[Machine, Unit] = (State.modify[Machine] _).compose(update)
        val x: State[Machine, Unit] = function(input)
        x
      }))
      potato
    }
    s <- State.get
  } yield (s.coins, s.candies)
}