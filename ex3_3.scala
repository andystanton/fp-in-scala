object Ex3_3 {
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case _ :: t => h :: t
    case _ => h :: Nil
  }

  def main(args: Array[String]) {
    assert(setHead(List(), 8) == List(8))
    assert(setHead(List(1), 8) == List(8))
    assert(setHead(List(1, 2), 8) == List(8, 2))
    assert(setHead(List(1, 2, 3), 8) == List(8, 2, 3))
    
    println("Ok!")
  }
}
