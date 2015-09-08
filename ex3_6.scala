object Ex3_6 {
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], build: List[A]): List[A] = l match {
      case h :: t if t.length > 1 => loop(t, build ++ List(h))
      case h :: t if t != Nil => build ++ List(h)
      case _ => Nil
    }

    loop(l, Nil)
  }

  def main(args: Array[String]) {
    assert(init(List(1, 2, 3)) == List(1, 2))
    assert(init(List(1, 2)) == List(1))
    assert(init(List(1)) == List())
    assert(init(List()) == List())

    println("Ok!")
  }
}