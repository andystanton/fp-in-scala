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
}
