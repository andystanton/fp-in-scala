package fp.functionalstate

import org.scalatest.{FlatSpec, Matchers}

class CandyMachineSpec extends FlatSpec with Matchers {
  // exercise 6.11
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

  it should "unlock when inserting a coin if it is locked and there are candies left" in {
    val machine = Machine(locked = true, candies = 5, coins = 10)
    val ((resultCoins, resultCandies), outMachine) = Machine.simulateMachine(List(Coin)).run(machine)

    resultCoins shouldBe 11
    resultCandies shouldBe 5
    outMachine.locked shouldBe false
  }

  it should "not unlock when inserting a coin if it is locked and there are no candies left" in {
    val machine = Machine(locked = true, candies = 0, coins = 10)
    val ((resultCoins, resultCandies), outMachine) = Machine.simulateMachine(List(Coin)).run(machine)

    resultCoins shouldBe 10
    resultCandies shouldBe 0
    outMachine.locked shouldBe true
  }

  it should "dispense candy and become locked when it is turned if it is unlocked" in {
    val machine = Machine(locked = false, candies = 8, coins = 10)
    val ((resultCoins, resultCandies), outMachine) = Machine.simulateMachine(List(Turn)).run(machine)

    resultCoins shouldBe 10
    resultCandies shouldBe 7
    outMachine.locked shouldBe true
  }

  it should "do nothing when it is turned on a locked machine" in {
    val machine = Machine(locked = true, candies = 8, coins = 10)
    val ((resultCoins, resultCandies), outMachine) = Machine.simulateMachine(List(Turn)).run(machine)

    resultCoins shouldBe 10
    resultCandies shouldBe 8
    outMachine.locked shouldBe true
  }

  it should "do nothing when inserting a coin on a unlocked machine" in {
    val machine = Machine(locked = false, candies = 8, coins = 10)
    val ((resultCoins, resultCandies), outMachine) = Machine.simulateMachine(List(Coin)).run(machine)

    resultCoins shouldBe 10
    resultCandies shouldBe 8
    outMachine.locked shouldBe false
  }

  it should "do nothing if there is no candy" in {
    val machine = Machine(locked = true, candies = 0, coins = 10)
    val ((resultCoins, resultCandies), outMachine) = Machine.simulateMachine(List(Coin)).run(machine)

    resultCoins shouldBe 10
    resultCandies shouldBe 0
    outMachine.locked shouldBe true
  }

//  it should "handle being run with no inputs" in {
//    intercept[IllegalArgumentException] {
//      val machine = Machine(locked = true, candies = 6, coins = 10)
//      Machine.simulateMachine(Nil: List[Input]).run(machine)
//    }.getMessage shouldBe "Cannot simulate machine with no inputs"
//  }
}
