package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def length[A](xs: List[A]): Int = xs match {
    case Cons(_, t) => length(t) + 1
    case _ => 0
  }

  def length2[A](xs: List[A]): Int = foldRight(xs, 0)((_, b) => b + 1)

  def sum(ns: List[Int]): Int = ns match {
    case Nil => 0
    case Cons(n, ns) => n + sum(ns)
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product(ns: List[Double]): Double = ns match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(n, ns) => n * product(ns)
  }

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def apply[A](xs: A*): List[A] =
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))

  // exercise 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(h, t) => t
    case _ => throw new IllegalArgumentException("Cannot return tail of nil")
  }

  // exercise 3.3
  def setHead[A](xs: List[A], x: A) = xs match {
    case Cons(_, t) => Cons(x, t)
    case _ => throw new IllegalArgumentException("Cannot set head of nil")
  }

  // exercise 3.4
  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Cons(_, t) if n > 1 => drop(t, n - 1)
    case Cons(_, t) => t
    case _ => throw new IllegalArgumentException("Cannot drop elements from nil")
  }

  // exercise 3.5
  def dropWhile[A](xs: List[A], p: A => Boolean): List[A] = xs match {
    case Nil => throw new IllegalArgumentException("Cannot drop elements from nil")
    case Cons(h, t) if p(h) => if (t == Nil) Nil else dropWhile(t, p)
    case _ => xs
  }

  def dropWhile2[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
    case Nil => throw new IllegalArgumentException("Cannot drop elements from nil")
    case Cons(h, t) if p(h) => if (t == Nil) Nil else dropWhile(t, p)
    case _ => xs
  }

  // exercise 3.6
  def init[A](xs: List[A]): List[A] = {
    def loop(xs: List[A]): List[A] = xs match {
      case Cons(h, t) if t != Nil => List.append(List(h), loop(t))
      case _ => Nil
    }
    xs match {
      case Nil => throw new IllegalArgumentException("Cannot return init of nil")
      case Cons(_, Nil) => throw new IllegalArgumentException("Cannot return init of list containing single item")
      case _ => loop(xs)
    }
  }

  // exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(h, z))(f)
  }

  // exercise 3.11
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def length3[A](xs: List[A]): Int = foldLeft(xs, 0)((_, b) => b + 1)
}
