package fp.errorhandling

object Variance {
  // exercise 4.2
  def mean(xs: Seq[BigDecimal]): Option[BigDecimal] =
    if (xs.nonEmpty) Some(xs.foldLeft(BigDecimal(0))(_+_) / BigDecimal(xs.length))
    else None

  def variance(xs: Seq[BigDecimal]): Option[BigDecimal] =
    mean(xs).flatMap(m => mean(xs.map(x => (x - m).pow(2))))
}
