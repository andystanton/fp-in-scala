package fpinscala.errorhandling

object Variance {
  // exercise 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (!xs.isEmpty) Some(xs.sum.toDouble / xs.length)
    else None

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
