package fpinscala.laziness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  override def toString: String = this match {
    case Empty => "Empty"
    case Cons(h, t) => s"Cons(${h()}, ${t().toString})"
  }

  def ===(other: Stream[Any]): Boolean = other match {
    case sameType: Stream[A] => this match {
      case Cons(h, t) => other match {
        case Cons(oh, ot) => h() == oh() && t() === ot()
        case _ => false
      }
      case _ => other == Empty
    }
    case _ => false
  }

  def reverse: Stream[A] = {
    def loop(as: Stream[A], out: Stream[A]): Stream[A] = as match {
      case Cons(h, t) => loop(t(), Cons(h, () => out))
      case _ => out
    }
    loop(this, Empty)
  }

  // exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListViaRecursion: List[A] = {
    @annotation.tailrec
    def loop(l: List[A], s: Stream[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => loop(h() :: l, t())
    }
    loop(Nil: List[A], this).reverse
  }

  // exercise 5.2
  def take(n: Int): Stream[A] = {
    def loop(n: Int, out: Stream[A], as: Stream[A]): Stream[A] = as match {
      case Cons(h, t) if n > 0 => loop(n - 1, Cons(h, () => out), t())
      case _ => out
    }
    loop(n, Empty, this).reverse
  }

  def drop(n: Int): Stream[A] = {
    def loop(n: Int, out: Stream[A], as: Stream[A]): Stream[A] = as match {
      case Cons(h, t) if n > 0 => loop(n - 1, out, t())
      case Cons(h, t) => loop(n - 1, Cons(h, () => out), t())
      case _ => out
    }
    loop(n, Empty, this).reverse
  }

  // exercise 5.3
  def takeWhile(f: A => Boolean): Stream[A] = {
    def loop(f: A => Boolean, out: Stream[A], as: Stream[A]): Stream[A] = as match {
      case Cons(h, t) if f(h()) => loop(f, Cons(h, () => out), t())
      case _ => out
    }
    loop(f, Empty, this).reverse
  }

  @annotation.tailrec
  final def foldLeft[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)
    case _ => z
  }

  final def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // exercise 5.4
  def forAllViaFoldRight(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  def forAll(f: A => Boolean): Boolean = this match {
    case Cons(h, t) if f(h()) => t().forAll(f)
    case Cons(h, t) => false
    case _ => true
  }

  // exercise 5.5
  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) =>
      if (f(a)) Stream.cons(a, b) else b)

  // exercise 5.6
  def headOptionViaFoldRight(): Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a).orElse(b))

  // exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) =>
      if (f(a)) Stream.cons(a, b.filter(f)) else b.filter(f))

  def append[B >: A](n: => Stream[B]): Stream[B] =
    foldRight(n)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // exercise 5.8
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  // exercise 5.9
  def from(a: Int): Stream[Int] =
    Stream.cons(a, from(a + 1))

  // exercise 5.10
  def fibs: Stream[Int] = {
    def fibs(i: Int, j: Int): Stream[Int] = Stream.cons(i, fibs(j, i + j))
    fibs(0, 1)
  }

  // exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case _ => Stream.empty
  }
}
