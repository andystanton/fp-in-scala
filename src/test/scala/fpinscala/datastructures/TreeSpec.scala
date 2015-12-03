package fpinscala.datastructures

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
  // exercise 3.25
  "A Tree companion object" should "calculate the number of nodes in a tree" in {
    Tree.size(Leaf()) shouldBe 1
    Tree.size(Branch(Leaf(), Leaf())) shouldBe 3
    Tree.size(Branch(Branch(Leaf(), Leaf()), Branch(Leaf(), Leaf()))) shouldBe 7
  }

  // exercise 3.26
  it should "find the maximum element in a tree of integers" in {
    Tree.maximum(Leaf[Int](3)) shouldBe 3
    Tree.maximum(Branch(Leaf(2), Leaf(3))) shouldBe 3
    Tree.maximum(Branch(Branch(Leaf(-2), Leaf(8)), Leaf(12))) shouldBe 12
  }

  // exercise 3.27
  it should "find the depth of a tree" in {
    Tree.depth(Leaf()) shouldBe 1
    Tree.depth(Branch(Leaf(), Leaf())) shouldBe 2
    Tree.depth(Branch(Branch(Leaf(), Leaf()), Branch(Branch(Leaf(), Leaf()), Leaf()))) shouldBe 4
  }
}
