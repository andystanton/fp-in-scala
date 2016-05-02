package fpinscala.functionalstate

trait RNG {
  def nextInt: (Int, RNG)
}

