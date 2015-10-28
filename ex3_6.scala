object Ex3_6 {
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], build: List[A]): List[A] = l match {
      case h :: t if l.length > 1 => loop(t, h :: build)
      case _ => build
    }
    
    @annotation.tailrec
    def reverse(l: List[A], build: List[A]): List[A] = l match {
      case h :: t => reverse(t, h :: build)
      case _ => build
    }

    reverse(loop(l, Nil), Nil)
  }

  def main(args: Array[String]) {
    assert(init(List(1, 2, 3)) == List(1, 2))
    assert(init(List(1, 2)) == List(1))
    assert(init(List(1)) == List())
    assert(init(List()) == List())

    println("Ok!")
  }
}