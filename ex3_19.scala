object Ex3_19 {
  
  def filter[A](xs: List[A])(predicate: A => Boolean) = {
    @annotation.tailrec
    def loop(xs: List[A], out: List[A])(predicate: A => Boolean): List[A] = xs match {
      case h :: t if predicate(h) => loop(t, h :: out)(predicate)
      case h :: t => loop(t, out)(predicate)
      case _ => out
    }
    
    @annotation.tailrec
    def reverse(xs: List[A], out: List[A]): List[A] = xs match {
      case h :: t => reverse(t, h :: out)
      case _ => out
    }
    
    reverse(loop(xs, Nil)(predicate), Nil)
  }
  
  def main(args: Array[String]) {
    assert(filter(List(1,2,3,4,5,6,7,8,9,10))(_%2==0) == List(2,4,6,8,10))
    
    println("Ok!")
  }
}