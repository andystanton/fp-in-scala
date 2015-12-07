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
  def depth[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => 1 + depth(left).max(depth(right))
    case Leaf(_) => 1
  }

  // exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value) => Leaf(f(value))
  }

  // exercise 3.29
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    case Leaf(value) => f(value)
  }

  def sizeViaFold[A](tree: Tree[A]) = fold(tree)(_ => 1)(_ + _ + 1)
  def depthViaFold[A](tree: Tree[A]) = fold(tree)(_ => 1)(_.max(_) + 1)
  def maximumViaFold(tree: Tree[Int]) = fold(tree)(identity)(_.max(_))
  def mapViaFold[A, B](tree: Tree[A])(f: A => B) = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
