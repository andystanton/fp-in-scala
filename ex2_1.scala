object Ex2_1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, a: Int, b: Int): Int = if (n <= 0) a + b else loop(n - 1, b, a + b)

    n match {
      case 1 => 0
      case 2 => 1
      case _ => loop(n - 3, 0, 1)
    }
  }

  def main(args: Array[String]) {
    assert((1 to 10).map(fib) == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))

    println("Ok!")
  }
}
