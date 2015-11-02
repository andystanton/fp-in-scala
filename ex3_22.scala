object Ex3_22 {
  def main(args: Array[String]) {
    assert(listSmasher(List(1,2,3), List(4,5,6)) == List(5, 7, 9))
    println("Ok!")
  }
  
  def listSmasher(xs: List[Int], ys: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(xs: List[Int], ys: List[Int], build: List[Int]): List[Int] = {
      xs match {
        case xh :: xt => ys match {
          case yh :: yt => loop(xt, yt, xh + yh :: build)
          case _ => build
        }
        case _ => build
      }
    }
    
    reverse(loop(xs, ys, Nil))
  }
  
  def reverse[A](xs: List[A]): List[A] = {
    @annotation.tailrec
    def loop(xs: List[A], out: List[A]): List[A] = xs match {
      case h :: t => loop(t, h :: out)
      case _ => out
    }
    loop(xs, Nil)
  }
}