object Ex3_11 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  def sum[A](as: List[A])(implicit num: Numeric[A]): A =
    foldLeft(as, num.zero)(num.plus(_, _))

  def product[A](as: List[A])(implicit num: Numeric[A]): A = 
    foldLeft(as, num.one)(num.times(_, _))

  def length[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

  def main(args: Array[String]) {
    val intslist = List(
      List(1),
      List(1, 2),
      List(1, 2, 3),
      List(1, 2, 3, 4))

    val doubleslist = List(
      List(1.0),
      List(1.0, 2.0),
      List(1.0, 2.0, 3.0),
      List(1.0, 2.0, 3.0, 4.0))
      
    intslist.foreach(ints => {
      assert(sum(ints) == ints.sum)
      assert(product(ints) == ints.product)
      assert(length(ints) == ints.length)
    })
    
    doubleslist.foreach(doubles => {
      assert(sum(doubles) == doubles.sum)
      assert(product(doubles) == doubles.product)
      assert(length(doubles) == doubles.length)
    })

    println("Ok!")
  }
}