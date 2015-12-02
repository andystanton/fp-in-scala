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
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // exercise 3.11
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def length3[A](xs: List[A]): Int = foldLeft(xs, 0)((b, _) => b + 1)

  // exerise 3.12
  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A])((a, b) => Cons(b, a))

  // exercise 3.13
  def foldLeft2[A, B](xs: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(xs), z)((a, b) => f(b, a))

  def foldRight2[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(xs), z)((b, a) => f(a, b))

  // exercise 3.14
  def append2[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)((a, z) => Cons(a, z))

  def append3[A](as: List[A], bs: List[A]): List[A] =
    foldLeft(reverse(as), bs)((z, a) => Cons(a, z))

  // exercise 3.15
  def concatenate[A](ls: List[List[A]]): List[A] =
    foldLeft(ls, Nil: List[A])((z, a) => append3(z, a))

  // exercise 3.16
  def addOne(ns: List[Int]): List[Int] =
    foldLeft(reverse(ns), Nil: List[Int])((z, a) => Cons(a + 1, z))

  // exercise 3.17
  def doubleToString(ns: List[Double]): List[String] =
    foldLeft(reverse(ns), Nil: List[String])((z, a) => Cons(a.toString, z))

  // exercise 3.18
  def map[A, B](xs: List[A])(f: A => B): List[B] =
    foldLeft(reverse(xs), Nil: List[B])((z, a) => Cons(f(a), z))

  // exercise 3.19
  def filter[A](xs: List[A])(f: A => Boolean): List[A] =
    foldLeft(reverse(xs), Nil: List[A])((z, a) => if (!f(a)) Cons(a, z) else z)

  // exercise 3.20
  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
    foldLeft(reverse(xs), Nil: List[B])((z, a) => append3(f(a), z))

  // exercise 3.21
  def filter2[A](xs: List[A])(f: A => Boolean): List[A] =
    flatMap(xs)(a => if (!f(a)) List(a) else Nil: List[A])

  // exercise 3.22
  def add(as: List[Int], bs: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(as: List[Int], bs: List[Int], z: List[Int]): List[Int] = as match {
      case Cons(ha, ta) => bs match {
        case Cons(hb, tb) => loop(ta, tb, Cons(ha + hb, z))
        case _ => z
      }
      case _ => z
    }
    loop(reverse(as), reverse(bs), Nil: List[Int])
  }

  // exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C) = {
    @annotation.tailrec
    def loop(as: List[A], bs: List[B], z: List[C]): List[C] = as match {
      case Cons(ha, ta) => bs match {
        case Cons(hb, tb) => loop(ta, tb, Cons(f(ha, hb), z))
        case _ => z
      }
      case _ => z
    }
    loop(reverse(as), reverse(bs), Nil: List[C])
  }

  // exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    foldLeft(reverse(sup), false)((z, a) =>
      foldLeft(reverse(sub), z)((y, b) => false))
}
