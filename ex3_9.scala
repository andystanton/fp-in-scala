object Ex3_9 {
  def foldRight[A, B](as: List[A], b: B)(f: (A, B) => B): B = as match {
    case h :: t => f(h, foldRight(t, b)(f))
    case _ => b
  }
  
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  def main(args: Array[String]) {
    assert(length(Nil) == 0)
    assert(length(List("a")) == 1)
    assert(length(List("a", "b")) == 2)
    assert(length(List("a", "b", "c")) == 3)

    println("Ok!")
  }
}