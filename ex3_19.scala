object Ex3_19 {
  
  def filter[A](xs: List[A])(predicate: A => Boolean) = {
    @annotation.tailrec
    def loop[C](xs: List[C], out: List[C])(predicate: C => Boolean): List[C] = {
      xs match {
        case h :: t if predicate(h) => loop(t, h :: out)(predicate)
        case h :: t => loop(t, out)(predicate)
        case _ => out
      }
    }
    
    @annotation.tailrec
    def reverse[B](xs: List[B], out: List[B]): List[B] = {
      xs match {
        case h :: t => reverse(t, h :: out)
        case _ => out
      }
    }
    
    reverse(loop(xs, Nil)(predicate), Nil)
  }
  
  def main(args: Array[String]) {
    println(filter(List(1,2,3,4,5,6,7,8,9,10))(_%2==0))
  }
}