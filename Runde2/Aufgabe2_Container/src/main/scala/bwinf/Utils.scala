package de.voodle.tim.bwinf.container

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Utils {
  type Cycle = List[Int]
  type Cycles = List[Cycle]

  import scala.util.Random
  def randPerm(n: Int) = {
    val a = (0 until n).toArray // O(n)
    for(i <- n to 2 by -1) { // O(n)
      val di = Random.nextInt(i)
      val swap = a(di)
      a(di) = a(i-1)
      a(i-1) = swap
    }
    a map (_ + 1) // from 0..n -> 1...n
  }

  /** Return the list of disjunct cycles sorted by cycle.head */
  def cyclesOf(perm: Seq[Int]): List[List[Int]] = {
    cycles(new ListBuffer[List[Int]], perm, new Array[Boolean](perm.length))
  }
  
  @tailrec
  private def cycles(ready: ListBuffer[List[Int]], perm: Seq[Int], handled: Array[Boolean], start: Int = 1): List[List[Int]] = // c * 
    if(start > perm.length || start < 1) ready.toList
    else {
      val aCycle = cycle(perm, start) // O(n_c)
      for (i <- aCycle) { handled(i-1) = true } // O(n_c); Side effects not harmful, it's tailrec!
      //val newHandled = handled ++ aCycle // O(n_c * hash)
      val next = (start to perm.length) findIndexOf (i => !(handled(i-1))) // -> find; and without +start
      if(next >= 0)
        cycles(ready += aCycle, perm, handled, next+start)
      else
        (ready += aCycle).toList // O(n)
    }
  /** Small helper function, finding one cycle. */
  private def cycle(perm: Seq[Int], start: Int): List[Int] = // O(n_c)
    (start +: helper(new ListBuffer[Int], perm, start)(perm(start - 1)))

  /** (tail)-recursive function, helping finding the cycle. */
  @tailrec
  private def helper(ready: ListBuffer[Int], perm: Seq[Int], start: Int)(idx: Int): Cycle = // O(n_c)
    if(start == idx) ready.toList // Reverse it! | O(n)
    else helper(ready += idx, perm, start)(perm(idx - 1))
}
