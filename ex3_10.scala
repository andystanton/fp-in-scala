object Ex3_10 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case h :: t => foldLeft(t, f(z, h))(f)
  }

  def main(args: Array[String]) {
    assert(foldLeft(Nil: List[Int], 0)(_ + _) == 0)
    assert(foldLeft(List(1), 0)(_ + _) == 1)
    assert(foldLeft(List(1, 2), 0)(_ + _) == 3)
    assert(foldLeft(List(1, 2, 3), 0)(_ + _) == 6)
    
    assert(foldLeft(Nil: List[Int], "")(_ + _) == "")
    assert(foldLeft(List(1), "")(_ + _) == "1")
    assert(foldLeft(List(1, 2), "")(_ + _) == "12")
    assert(foldLeft(List(1, 2, 3), "")(_ + _) == "123")

    println("Ok!")
  }
}