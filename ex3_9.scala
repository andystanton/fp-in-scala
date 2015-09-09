object Ex3_9 {
  def length[A](as: List[A]): Int = as.foldRight(0)((_, b) => b + 1)

  def main(args: Array[String]) {
    assert(length(Nil) == 0)
    assert(length(List("a")) == 1)
    assert(length(List("a", "b")) == 2)
    assert(length(List("a", "b", "c")) == 3)

    println("Ok!")
  }
}