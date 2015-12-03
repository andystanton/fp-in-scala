package fpinscala.datastructures

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
  // exercise 3.25
  "A Tree companion object" should "calculate the number of nodes in a tree" in {
    Tree.size(Leaf()) shouldBe 1
    Tree.size(Branch(Leaf(), Leaf())) shouldBe 3
    Tree.size(Branch(Branch(Leaf(), Leaf()), Branch(Leaf(), Leaf()))) shouldBe 7
  }
}
