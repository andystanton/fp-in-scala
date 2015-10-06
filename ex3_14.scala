object Ex3_14 {
  def append[A](as: List[A], a: A): List[A] = {
    as.foldLeft(List(a))((a, b) => a match {
        case h :: t => h :: b :: t
        case _ => Nil
      }).foldLeft(Nil: List[A])((a, b) => (b :: a))
  }

  def main(args: Array[String]) {
    println(append(List(1, 2, 3, 4), 5))
  }
}