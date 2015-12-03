package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // exercise 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case Leaf(_) => 1
  }

  // exercise 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(left, right) => maximum(left).max(maximum(right))
    case Leaf(value) => value
  }

  // exercise 3.27
  def depth[A](tree: Tree[A]): Int = {
    def loop(tree: Tree[A], carry: Int): Int = tree match {
      case Branch(left, right) => loop(left, carry + 1).max(loop(right, carry + 1))
      case Leaf(_) => carry + 1
    }
    loop(tree, 0)
  }
}
