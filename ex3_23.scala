object Ex3_23 {
  def main(args: Array[String]) {
    assert(zipWith(List(1,2,3), List(4,5,6))(_+_) == List(5, 7, 9))
    assert(zipWith(List("a","b","c"), List("x","y","z"))(_+_) == List("ax", "by", "cz"))
    println("Ok!")
  }
  
  def zipWith[A, B](xs: List[A], ys: List[A])(f: (A, A) => B): List[B] = {
    @annotation.tailrec
    def loop(xs: List[A], ys: List[A], build: List[B]): List[B] = {
      xs match {
        case xh :: xt => ys match {
          case yh :: yt => loop(xt, yt, f(xh, yh) :: build)
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