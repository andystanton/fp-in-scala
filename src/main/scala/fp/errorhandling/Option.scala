package fp.errorhandling

import fp.datastructures._

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  // exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case _ => None
  }

  def getOrElse[B >: A](default: B): B = this match {
    case Some(get) => get
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  // exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(ia => b.map(ib => f(ia, ib)))

  // exercise 4.4
  def sequence[A](xs: List[Option[A]]): Option[List[A]] = xs match {
    case Cons(h, t) => h.flatMap(x => sequence(t).map(y => Cons(x, y)))
    case Nil => Some(Nil)
  }

  def sequenceViaMap2[A](xs: List[Option[A]]): Option[List[A]] =
    List.foldLeft(List.reverse(xs), Some(Nil): Option[List[A]])((z, a) => map2(a, z)(Cons(_, _)))

  // exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(List.map(a)(f))

  def traverseViaRecursion[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Cons(h, t) => f(h).flatMap(x => traverseViaRecursion(t)(f).map(y => Cons(x, y)))
    case Nil => Some(Nil)
  }

  def sequenceViaTraverse[A](xs: List[Option[A]]): Option[List[A]] =
    traverseViaRecursion(xs)(identity)
}
