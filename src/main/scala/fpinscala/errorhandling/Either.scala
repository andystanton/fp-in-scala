package fpinscala.errorhandling

import fpinscala.datastructures._

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
