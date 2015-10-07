object Ex3_16 {
  def add1(xs: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(xs: List[Int], out: List[Int]): List[Int] = {
      xs match {
        case Nil => out
        case h :: t => loop(t, h + 1 :: out)
      }
    }
    
    @annotation.tailrec
    def reverse[A](xs: List[A], out: List[A]): List[A] = {
      xs match {
        case Nil => out
        case h :: t => reverse(t, h :: out)
      }
    }
    
    reverse(loop(xs, Nil), Nil)
  }
  
  def main(args: Array[String]) {
    assert(add1(List(1,2,3)) == List(2,3,4))
    
    println("Ok!")
  }
}



