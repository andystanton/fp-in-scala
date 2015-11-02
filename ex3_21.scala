object Ex3_21 {
  def main(args: Array[String]) {
    assert(flatmapFilter(List(1,2,3,4,5,6,7,8,9,10))(_%2==0) == List(2,4,6,8,10))
    assert(flatmapFilter(List(1,2,3,4,5,6,7,8,9,10))(_<6) == List(1,2,3,4,5))
    println("Ok!")
  }
  
  def flatmapFilter[A](xs: List[A])(p: A => Boolean): List[A] = {
    def listfilterer(x: A) = if (p(x)) List(x) else Nil
    flatmap(xs)(listfilterer)
  }
  
  def reverse[A](xs: List[A]): List[A] = {
    @annotation.tailrec
    def loop(xs: List[A], out: List[A]): List[A] = xs match {
      case h :: t => loop(t, h :: out)
      case _ => out
    }
    
    loop(xs, Nil)
  }
  
  def concatenate[A](xs: List[A], ys: List[A]): List[A] = {
    @annotation.tailrec
    def loop(xs: List[A], ys: List[A]): List[A] = xs match {
      case h :: t => loop(t, h :: ys)
      case _ => ys
    }
    loop(reverse(xs), ys)
  }
  
  def filter[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(xs: List[A], out: List[A])(predicate: A => Boolean): List[A] = xs match {
      case h :: t if predicate(h) => loop(t, h :: out)(predicate)
      case h :: t => loop(t, out)(predicate)
      case _ => out
    }

    reverse(loop(xs, Nil)(predicate))
  }
  
  def flatmap[A, B](xs: List[A])(f: A => List[B]): List[B] = {  
    @annotation.tailrec
    def loop(xs: List[A], out: List[B]): List[B] = xs match {
      case Nil => out
      case h :: t => loop(t, concatenate(f(h), out))
    }
    loop(reverse(xs), Nil)
  }
}