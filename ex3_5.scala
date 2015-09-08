object Ex3_5 {
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case h :: t if f(h) => dropWhile(t, f)
    case _ => l
  }
  
  @annotation.tailrec
  def betterDropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case h :: t if f(h) => betterDropWhile(t)(f)
    case _ => l
  }

  def main(args: Array[String]) {
    assert(dropWhile(List(1, 2, 3, 2, 1), (x: Int) => x < 1) == List(1, 2, 3, 2, 1))
    assert(dropWhile(List(1, 2, 3, 2, 1), (x: Int) => x < 2) == List(2, 3, 2, 1))
    assert(dropWhile(List(1, 2, 3, 2, 1), (x: Int) => x < 3) == List(3, 2, 1))
    assert(dropWhile(List(1, 2, 3, 2, 1), (x: Int) => x < 4) == List())
    assert(dropWhile(List(), (x: Int) => x < 4) == List())
    
    assert(betterDropWhile(List(1, 2, 3, 2, 1))(x => x < 1) == List(1, 2, 3, 2, 1))
    assert(betterDropWhile(List(1, 2, 3, 2, 1))(x => x < 2) == List(2, 3, 2, 1))
    assert(betterDropWhile(List(1, 2, 3, 2, 1))(x => x < 3) == List(3, 2, 1))
    assert(betterDropWhile(List(1, 2, 3, 2, 1))(x => x < 4) == List())
    assert(betterDropWhile(List[Int]())(x => x < 4) == List())

    println("Ok!")
  }
}
