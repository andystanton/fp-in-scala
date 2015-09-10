object Ex3_13 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as.length match {
    case 0 => z
    case _ => foldLeft(as.tail, f(as.head, z))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as.length match {
    case 0 => z
    case _ => f(as.head, foldRight(as.tail, z)(f))
  }
  
  def cheatFoldLeftToRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = 
    foldRight(as.reverse, z)(f)
    
  def cheatFoldRightToLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = 
    foldLeft(as.reverse, z)(f)

  def main(args: Array[String]) {
    println(foldLeft(List("1", "2", "3"), "")(_+_))
    println(foldRight(List("1", "2", "3"), "")(_+_))
    println(cheatFoldLeftToRight(List("1", "2", "3"), "")(_+_))
    println(cheatFoldRightToLeft(List("1", "2", "3"), "")(_+_))
    println("Ok!")
  }
}