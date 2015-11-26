package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
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
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case Cons(_, _) => xs
    case _ => throw new IllegalArgumentException("Cannot drop elements from nil")
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // exercise 3.6
  def init[A](xs: List[A]): List[A] = xs match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => List.append(List(h), init(t))
    case _ => throw new IllegalArgumentException("Cannot return init of nil")
  }
}
