object Ex3_14 {
  def append[A](as: List[A], a: A): List[A] = {
    as.foldLeft(List(a))((x: List[A], y: A) => {
      x ++ List(y)
    })
  }

  def main(args: Array[String]) {
    println(append(List(1, 2, 3), 4))
  }
}