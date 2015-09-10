object Ex3_12 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as.length match {
    case 0 => z
    case _ => foldLeft(as.tail, f(as.head, z))(f)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((a, b) => a :: b)
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