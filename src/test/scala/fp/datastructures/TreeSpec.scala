package fp.datastructures

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
  // exercise 3.25
  "A Tree companion object" should "calculate the number of nodes in a tree" in {
    Tree.size(Leaf(())) shouldBe 1
    Tree.size(Branch(Leaf(()), Leaf(()))) shouldBe 3
    Tree.size(Branch(Branch(Leaf(()), Leaf(())), Branch(Leaf(()), Leaf(())))) shouldBe 7
  }

  // exercise 3.26
  it should "find the maximum element in a tree of integers" in {
    Tree.maximum(Leaf(3)) shouldBe 3
    Tree.maximum(Branch(Leaf(2), Leaf(3))) shouldBe 3
    Tree.maximum(Branch(Branch(Leaf(-2), Leaf(8)), Leaf(12))) shouldBe 12
  }

  // exercise 3.27
  it should "find the depth of a tree" in {
    Tree.depth(Leaf(())) shouldBe 1
    Tree.depth(Branch(Leaf(()), Leaf(()))) shouldBe 2
    Tree.depth(Branch(Branch(Leaf(()), Leaf(())), Branch(Branch(Leaf(()), Leaf(())), Leaf(())))) shouldBe 4
  }

  // exercise 3.28
  it should "convert a tree from one type to another" in {
    Tree.map(Branch(Leaf(1), Leaf(2)))(_ * 3) shouldBe Branch(Leaf(3), Leaf(6))
    Tree.map(Branch(Leaf(1), Leaf(2)))(_ + 3) shouldBe Branch(Leaf(4), Leaf(5))
    Tree.map(Branch(Leaf(1), Leaf(2)))(_.toString) shouldBe Branch(Leaf("1"), Leaf("2"))
  }

  // exercise 3.29
  it should "calculate the number of nodes in a tree using fold" in {
    Tree.sizeViaFold(Leaf(())) shouldBe 1
    Tree.sizeViaFold(Branch(Leaf(()), Leaf(()))) shouldBe 3
    Tree.sizeViaFold(Branch(Branch(Leaf(()), Leaf(())), Branch(Leaf(()), Leaf(())))) shouldBe 7
  }

  it should "find the maximum element in a tree of integers using fold" in {
    Tree.maximum(Leaf(3)) shouldBe 3
    Tree.maximum(Branch(Leaf(2), Leaf(3))) shouldBe 3
    Tree.maximum(Branch(Branch(Leaf(-2), Leaf(8)), Leaf(12))) shouldBe 12
  }

  it should "find the depth of a tree using fold" in {
    Tree.depth(Leaf(())) shouldBe 1
    Tree.depth(Branch(Leaf(()), Leaf(()))) shouldBe 2
    Tree.depth(Branch(Branch(Leaf(()), Leaf(())), Branch(Branch(Leaf(()), Leaf(())), Leaf(())))) shouldBe 4
  }

  it should "convert a tree from one type to another using fold" in {
    Tree.map(Branch(Leaf(1), Leaf(2)))(_ * 3) shouldBe Branch(Leaf(3), Leaf(6))
    Tree.map(Branch(Leaf(1), Leaf(2)))(_ + 3) shouldBe Branch(Leaf(4), Leaf(5))
    Tree.map(Branch(Leaf(1), Leaf(2)))(_.toString) shouldBe Branch(Leaf("1"), Leaf("2"))
  }
}
