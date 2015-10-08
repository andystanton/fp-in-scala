object Ex3_20 {
  def flatmap[A, B](xs: List[A])(f: A => List[B]): List[B] = {
    def concatenate[C](xs: List[C], ys: List[C]): List[C] = {
      @annotation.tailrec
      def loop(xs: List[C], ys: List[C]): List[C] = xs match {
        case h :: t => loop(t, h :: ys)
        case _ => ys
      }
      loop(reverse(xs), ys)
    }

    def reverse[C](xs: List[C]): List[C] = xs.foldLeft(Nil: List[C])((a, b) => b :: a)
    

    def loop(xs: List[A], out: List[B]): List[B] = xs match {
      case Nil => out
      case h :: t => loop(t, concatenate(f(h), out))
    }
    loop(reverse(xs), Nil)
  }

  def main(args: Array[String]) {
    def stringExploder(str: String) = str.toCharArray.toList.map(_.toString)
    assert(flatmap(List("hello", "world"))(stringExploder) ==
      List("h", "e", "l", "l", "o", "w", "o", "r", "l", "d"))
    assert(flatmap(List(1,2,3))(i => List(i, i)) == List(1,1,2,2,3,3))
    println("Ok!")
  }
}