object Ex3_10 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as.length match {
      case 0 => z
      case _ => foldLeft(as.tail, f(as.head, z))(f)
    }
  }

  def main(args: Array[String]) {
    assert(foldLeft(Nil: List[Int], 0)(_ + _) == 0)
    assert(foldLeft(List(1), 0)(_ + _) == 1)
    assert(foldLeft(List(1, 2), 0)(_ + _) == 3)
    assert(foldLeft(List(1, 2, 3), 0)(_ + _) == 6)

    assert(foldLeft(Nil: List[Int], "")(_ + _) == "")
    assert(foldLeft(List(1), "")(_ + _) == "1")
    assert(foldLeft(List(1, 2), "")(_ + _) == "21")
    assert(foldLeft(List(1, 2, 3), "")(_ + _) == "321")

    println("Ok!")
  }
}