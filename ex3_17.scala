object Ex3_17 {
  def doublesToString(xs: List[Double]): List[String] = {
    @annotation.tailrec
    def loop(xs: List[Double], out: List[String]): List[String] = {
      xs match {
        case Nil => out
        case h :: t => loop(t, h.toString() :: out)
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
    assert(doublesToString(List(1.0,2.0,3.0)) == List("1.0","2.0","3.0"))
    
    println("Ok!")
  }
}



