package de.voodle.tim.bwinf.container

import annotation.tailrec
import scala.collection.mutable.tim.ListBuffer // <-- custom ListBuffer

object Cycler {
  type Cycle = List[Int]
  type Cycles = List[List[Int]]
}

trait Cycler extends Function1[Seq[Int], List[List[Int]]] {
  def apply(perm: Seq[Int]) = cyclesOf(perm)
  def cyclesOf(perm: Seq[Int]): List[List[Int]]
}
import Cycler._

object FastCycler extends Cycler {
  /** Return the list of disjunct cycles sorted ascending by cycle.head */
  def cyclesOf(perm: Seq[Int]): Cycles = cyclesOf(new ListBuffer[Cycle], perm, new Array[Boolean](perm.length))

  @tailrec
  private def cyclesOf(ready: ListBuffer[Cycle], perm: Seq[Int], handled: Array[Boolean], start: Int = 1): Cycles = // c *
    if(start > perm.length || start < 1) ready.toList
    else {
      val aCycle = cycle(perm, start) // O(n_c)
      for (i <- aCycle) { handled(i-1) = true } // O(n_c); Side effects are not harmful, it's tailrec!
      (start to perm.length) find (i => !(handled(i-1))) match { // O(i_c) //\sum{i_c} = n
        case Some(next) =>
          cyclesOf(ready += aCycle, perm, handled, next)
        case None =>
          (ready += aCycle).toList // O(1)
      }
    }
  /** Small helper function, finding one cycle. */
  private def cycle(perm: Seq[Int], start: Int): Cycle = {// O(n_c)
    @tailrec def step(ready: ListBuffer[Int], idx: Int): Cycle = // O(n_c)
      if(start == idx)
        ready.toList // O(1)
      else
        step(ready += idx,perm(idx - 1))
    (start :: step(new ListBuffer[Int], perm(start - 1)))
  }
}

object SlowCycler extends Cycler {
  def cycle(perm: Seq[Int], start: Int): Cycle = {
    def step(idx: Int): Cycle = // Hilfsfunktion
      if(start == idx)
        Nil
      else
        idx :: step(perm(idx - 1))
    start :: step(perm(start-1))
  }
  def cyclesOf(perm: Seq[Int]): Cycles = cyclesOf(perm, Set())
  def cyclesOf(perm: Seq[Int], ready: Set[Int]): Cycles =
    (1 to perm.length) find (i => !ready.contains(i)) match { // bis zu n Aufrufe; Zugriff auf Hashset O(1)
      case Some(start) =>
        val newCycle = cycle(perm, start)
        val newReady = ready ++ newCycle // O(n)
        newCycle :: cyclesOf(perm, newReady)
      case None =>
        Nil
    }
}