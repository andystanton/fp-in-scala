object Ex3_13 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case h :: t => f(h, foldRight(t, z)(f))
  }

  def main(args: Array[String]) {
    println(foldLeft(List("1", "2", "3"), "")(_ + _))
    println(foldRight(List("1", "2", "3"), "")(_ + _))

    println("Ok!")
  }
}