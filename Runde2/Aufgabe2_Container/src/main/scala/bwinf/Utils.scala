package de.voodle.tim.bwinf.container

import scala.annotation.tailrec

object Utils {
  type Cycle = List[Int]
  type Cycles = List[Cycle]

  import scala.util.Random
  def psRandPerm(n: Int) = {
    val a = (0 until n).toArray
    for(i <- n to 2 by -1) {
      val di = Random.nextInt(i)
      val swap = a(di)
      a(di) = a(i-1)
      a(i-1) = swap
    }
    a map (_ + 1) // from 0..n -> 1...n
  }

  /** Return the list of disjunct cycles sorted by cycle.head */
  def cyclesOf(perm: Seq[Int]) = cycles(Nil, perm)
  
  @tailrec
  private def cycles(ready: Cycles, perm: Seq[Int], handled: Set[Int] = Set(), start: Int = 1): Cycles = // c * 
    if(start > perm.length || start < 1) ready.reverse
    else {
      val aCycle = cycle(perm, start) // O(n)
      val newHandled = handled ++ aCycle
      val next = (start to perm.length) findIndexOf (i => !(newHandled contains i)) // O(n * log n)
      if(next >= 0)
        cycles(aCycle :: ready, perm, newHandled, next+start)
      else
        (aCycle :: ready).reverse // O(n)
    }
  /** Small helper function, finding one cycle. */
  private def cycle(perm: Seq[Int], start: Int) = // O(n_c)
    (start :: helper(Nil, perm, start)(perm(start - 1)))

  @tailrec
  private def helper(ready: List[Int], perm: Seq[Int], start: Int)(idx: Int): Cycle = // O(n_c)
    if(start == idx) ready.reverse // Reverse it! | O(n)
    else helper(idx :: ready, perm, start)(perm(idx - 1))
}
