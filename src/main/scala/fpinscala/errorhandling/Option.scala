package fpinscala.errorhandling

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
}
