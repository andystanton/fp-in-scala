package fp.errorhandling

import fp.datastructures._

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {
  // exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(error) => Left(error)
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(error) => Left(error)
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(ia => b.map(ib => f(ia, ib)))
}

object Either {
  // exercise 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Cons(h, t) => h.flatMap(x => sequence(t).map(xs => Cons(x, xs)))
    case _ => Right(Nil)
  }

  def sequenceViaFold[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    List.foldLeft(List.reverse(es), Right(Nil): Either[E, List[A]])((z, a) => a.map2(z)(Cons(_, _)))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Cons(h, t) => f(h).flatMap(x => traverse(t)(f).map(xs => Cons(x, xs)))
    case _ => Right(Nil)
  }

  def traverseViaFold[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    List.foldLeft(List.reverse(as), Right(Nil): Either[E, List[B]])((z, a) => f(a).map2(z)(Cons(_, _)))
}
