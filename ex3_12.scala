object Ex3_12 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((b, a) => a :: b)
  }

  def main(args: Array[String]) {
    assert(reverse(Nil: List[Int]) == (Nil: List[Int]).reverse)
    assert(reverse(List(1)) == List(1).reverse)
    assert(reverse(List(1, 2)) == List(1, 2).reverse)
    assert(reverse(List(1, 2, 3)) == List(1, 2, 3).reverse)
    assert(reverse(List(1, 2, 3, 4)) == List(1, 2, 3, 4).reverse)
    
    println("Ok!")
  }
}