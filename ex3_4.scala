object Ex3_4 {
  def drop(l: List[Int], n: Int): List[Int] = l match {
    case h :: t if (h < n) => drop(t, n)
    case _ => l
  }

  def main(args: Array[String]): Unit = {
    assert(drop(List(1,2,3,4,5), 3) == List(3,4,5))
    
    println("Ok!")
  }
}
