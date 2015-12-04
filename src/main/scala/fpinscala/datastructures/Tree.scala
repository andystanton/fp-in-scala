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

  // exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value) => Leaf(f(value))
  }

  // exercise 3.29
  def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B)(g: (Tree[A], B) => B)(h: (B, B) => B): B = tree match {
    case Branch(left, right) => h(fold(left, g(left, z))(f)(g)(h), fold(right, g(right, z))(f)(g)(h))
    case Leaf(value) => f(value, z)
  }

  def size2[A](tree: Tree[A]) = fold(tree, 0)((_, _) => 1)((_, _) => 1)(_ + _ + 1)
  def depth2[A](tree: Tree[A]) = fold(tree, 0)((_, z) => z + 1)((_, z) => z + 1)(_.max(_))
  def maximum2(tree: Tree[Int]) = fold(tree, Int.MinValue)((n, _) => n)((_, z) => z)(_.max(_))
  def map2[A, B](tree: Tree[A])(f: A => B) = fold(tree, null: Tree[B])((a, _) => Leaf(f(a)))((_, b) => b)(Branch(_, _))
}
