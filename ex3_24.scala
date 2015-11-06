object Ex3_24 {
  
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def innerloop(sup: List[A], 
                  sub: List[A], 
                  started: Boolean, 
                  inbuild: Boolean,
                  outbuild: Boolean): Boolean = {
      sup match {
        case h :: t => sub match {
          case hh :: tt if h == hh && started => innerloop(t, tt, true, inbuild && true, outbuild)
          case hh :: tt if h == hh && !started => innerloop(t, tt, true, true, outbuild)
          case hh :: tt if started => innerloop(t, tt, true, false, outbuild)
          case hh :: tt => innerloop(t, tt, false, false, outbuild)
          case _ => inbuild || outbuild
        }
        case _ => inbuild || outbuild
      }
    }
    
    @annotation.tailrec
    def outerloop(sup: List[A], sub: List[A], build: Boolean, firstTime: Boolean): Boolean = {
      sup match {
        case _ if firstTime => outerloop(sup, sub, innerloop(sup, sub, false, false, build), false)
        case h :: t => outerloop(t, sub, innerloop(sup, sub, false, false, build), false)
        case _ => build
      }
    }
    
    outerloop(sup, sub, false, true)
  }
  
  def reverse[A](xs: List[A]): List[A] = {
    @annotation.tailrec
    def loop(xs: List[A], out: List[A]): List[A] = xs match {
      case h :: t => loop(t, h :: out)
      case _ => out
    }
    loop(xs, Nil)
  }

  def main(args: Array[String]) {
    println("--true--")
    println(hasSubsequence(List(1,2,3,4), List(1,2)))
    println(hasSubsequence(List(1,2,3,4), List(2,3,4)))
    println(hasSubsequence(List(1,2,3,4), List(4)))
    println(hasSubsequence(List(1,2,3,4), List(1)))
    println(hasSubsequence(List(1,2,3,4), List(1,2,3,4)))
    println()
    println("--false--")
    println(hasSubsequence(List(1,2,3,4), List(4,3,2,1)))
    println(hasSubsequence(List(1,2,3,4), List(4,3)))
    println(hasSubsequence(List(1,2,3,4), List(8)))
  }
}