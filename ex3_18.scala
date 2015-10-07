object Ex3_18 {
  def map[A, B](xs: List[A])(f: A => B): List[B] = {
    @annotation.tailrec
    def loop(xs: List[A], out: List[B]): List[B] = {
      xs match {
        case Nil => out
        case h :: t => loop(t, f(h) :: out)
      }
    }
    
    @annotation.tailrec
    def reverse[C](xs: List[C], out: List[C]): List[C] = {
      xs match {
        case Nil => out
        case h :: t => reverse(t, h :: out)
      }
    }
    
    reverse(loop(xs, Nil), Nil)
  }
  
  def main(args: Array[String]) {
    assert(map(List(1,2,3))(_+1) == List(2,3,4))
    assert(map(List(1.0,2.0,3.0))(_.toString()) == List("1.0","2.0","3.0"))
    
    println("Ok!")
  }
}



