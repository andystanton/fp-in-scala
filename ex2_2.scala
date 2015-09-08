object Ex2_2 {
  @annotation.tailrec
  def isSorted[A](as: List[A], f: (A, A) => Boolean): Boolean = as.length match {
    case x if x > 2 => if (f(as.head, as.tail.head)) isSorted(as.tail, f) else false
    case 2 => f(as.head, as.tail.head)
    case _ => true
  }

  def main(args: Array[String]) {
    def sortIntAsc(a: Int, b: Int) = a < b
    def sortIntDesc(a: Int, b: Int) = a > b
    def sortStringAsc(a: String, b: String) = a < b
    def sortStringDesc(a: String, b: String) = a > b

    val foos = List(
      List(1, 2),
      List(1, 2, 3))

    val edgeFoos = List(
      List(),
      List(1))

    foos.foreach(foo => assert(isSorted(foo, sortIntAsc)))
    foos.foreach(foo => assert(!isSorted(foo, sortIntDesc)))
    edgeFoos.foreach(foo => assert(isSorted(foo, sortIntAsc)))
    edgeFoos.foreach(foo => assert(isSorted(foo, sortIntDesc)))

    foos.foreach(foo => assert(isSorted(foo.reverse, sortIntDesc)))
    foos.foreach(foo => assert(!isSorted(foo.reverse, sortIntAsc)))
    edgeFoos.foreach(foo => assert(isSorted(foo.reverse, sortIntDesc)))
    edgeFoos.foreach(foo => assert(isSorted(foo.reverse, sortIntAsc)))

    val bars = List(
      List("a", "b"),
      List("a", "b", "c"))

    val edgeBars = List(
      List(),
      List("a"))

    bars.foreach(bar => assert(isSorted(bar, sortStringAsc)))
    bars.foreach(bar => assert(!isSorted(bar, sortStringDesc)))
    edgeBars.foreach(bar => assert(isSorted(bar, sortStringAsc)))
    edgeBars.foreach(bar => assert(isSorted(bar, sortStringDesc)))

    bars.foreach(bar => assert(isSorted(bar.reverse, sortStringDesc)))
    bars.foreach(bar => assert(!isSorted(bar.reverse, sortStringAsc)))
    edgeBars.foreach(bar => assert(isSorted(bar.reverse, sortStringDesc)))
    edgeBars.foreach(bar => assert(isSorted(bar.reverse, sortStringAsc)))
    
    println("Ok!")
  }
}

