object Ex3_3 {
  def tail[A](xs: List[A]): List[A] = xs match {
    case _ :: t => t
    case _ => Nil
  }

  def main(args: Array[String]): Unit = {
    val foos = List(
      List(1),
      List(1,2),
      List(1,2,3)
    )
    
    foos.foreach(foo => assert(tail(foo) == foo.tail))

    println("Ok!")
  }
}
