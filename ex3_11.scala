object Ex3_11 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as.length match {
      case 0 => z
      case _ => foldLeft(as.tail, f(as.head, z))(f)
    }
  }

  def sum[A](as: List[A])(implicit num: Numeric[A]): A =
    foldLeft(as, num.zero)(num.plus(_, _))

  def product[A](as: List[A])(implicit num: Numeric[A]): A = as.length match {
    case 0 => num.zero
    case _ => foldLeft(as, num.one)(num.times(_, _))
  }

  def length[A](as: List[A]): Int = {
    foldLeft(as, 0)((_, b) => b + 1)
  }

  def main(args: Array[String]) {
    assert(sum(Nil: List[Int]) == 0)
    assert(sum(List(1)) == 1)
    assert(sum(List(1, 2)) == 3)
    assert(sum(List(1, 2, 3)) == 6)
    assert(sum(List(1, 2, 3, 4)) == 10)

    assert(sum(Nil: List[Double]) == 0.0)
    assert(sum(List(1.0)) == 1.0)
    assert(sum(List(1.0, 2.0)) == 3.0)
    assert(sum(List(1.0, 2.0, 3.0)) == 6.0)
    assert(sum(List(1.0, 2.0, 3.0, 4.0)) == 10.0)

    assert(product(Nil: List[Int]) == 0)
    assert(product(List(1)) == 1)
    assert(product(List(1, 2)) == 2)
    assert(product(List(1, 2, 3)) == 6)
    assert(product(List(1, 2, 3, 4)) == 24)

    assert(product(Nil: List[Double]) == 0.0)
    assert(product(List(1.0)) == 1.0)
    assert(product(List(1.0, 2.0)) == 2.0)
    assert(product(List(1.0, 2.0, 3.0)) == 6.0)
    assert(product(List(1.0, 2.0, 3.0, 4.0)) == 24)
    
    assert(length(Nil: List[Int]) == 0)
    assert(length(List(1)) == 1)
    assert(length(List(1, 2)) == 2)
    assert(length(List(1, 2, 3)) == 3)
    assert(length(List(1, 2, 3, 4)) == 4)
    
    assert(length(Nil: List[Double]) == 0)
    assert(length(List(1.0)) == 1)
    assert(length(List(1.0, 2.0)) == 2)
    assert(length(List(1.0, 2.0, 3.0)) == 3)
    assert(length(List(1.0, 2.0, 3.0, 4.0)) == 4)

    println("Ok!")
  }
}