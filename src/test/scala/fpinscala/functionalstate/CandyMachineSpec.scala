package fpinscala.functionalstate

import org.scalatest.{FlatSpec, Matchers}

class CandyMachineSpec extends FlatSpec with Matchers {
  "A Candy Machine" should "work" in {
    val machine = Machine(locked = true, candies = 5, coins = 10)
    val ((resultCoins, resultCandies), _) = Machine.simulateMachine(List(
      Coin,
      Turn,
      Coin,
      Turn,
      Coin,
      Turn,
      Coin,
      Turn
    )).run(machine)

    resultCoins shouldBe 14
    resultCandies shouldBe 1
  }
}
