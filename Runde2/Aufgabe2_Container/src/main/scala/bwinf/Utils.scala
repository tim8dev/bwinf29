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
    a map (_ + 1) toList // Drop first, because don't need the heading 0.
  }

  /* Must return a List of Cycles sorted by _.head */
  def cyclesOf(perm: Seq[Int]) = cycles(Nil, perm) //filterNot (_.tail.isEmpty)
  
  @tailrec
  private def cycles(ready: Cycles, perm: Seq[Int], handled: Set[Int] = Set(), start: Int = 1): Cycles = // c * 
    if(start > perm.length || start < 1) ready.reverse
    else {
      val trans = cycle(perm, start) // O(n)
      val newHandled = handled ++ trans
      val next = (start to perm.length) findIndexOf (i => !(newHandled contains i)) // O(n * log n)
      if(next >= 0)
        cycles(trans :: ready, perm, newHandled, next+start)
      else
        (trans :: ready).reverse // O(n)
    }
  private def cycle(perm: Seq[Int], start: Int) = { // O(n)
    val startIdx = start // - 1 // from 1...n -> 0..n
    val transPerm = perm //map (_ - 1)
    (startIdx :: helper(Nil, transPerm, startIdx)(transPerm(startIdx - 1))) // map (_ + 1) // from 0..n -> 1...n
  }
  @tailrec
  private def helper(ready: List[Int], perm: Seq[Int], start: Int)(idx: Int): Cycle = // | O(n)
    if(start == idx) ready.reverse // Reverse it! | O(n)
    else helper(idx :: ready, perm, start)(perm(idx - 1))
}
