object Ex3_15 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as.length match {
    case 0 => z
    case _ => foldLeft(as.tail, f(z, as.head))(f)
  }

  def squish[A](as: List[List[A]]): List[A] = {
    foldLeft(as, Nil: List[A])((xs: List[A], x: List[A]) => {
      
      xs ++ x
    })
  }

  def main(args: Array[String]) {
    val foo = List(
      List("1", "2", "3"),
      List("a", "b", "c"),
      List("x", "y", "z"))

    println(squish(foo))
  }
}