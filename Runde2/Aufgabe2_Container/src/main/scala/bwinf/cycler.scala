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
  def cyclesOf(perm: Seq[Int], handled: Set[Int]): Cycles =
    (1 to perm.length) find (i => !handled.contains(i)) match { // bis zu n Aufrufe; Zugriff auf Hashset O(1)
      case Some(start) =>
        val newCycle = cycle(perm, start)
        val newReady = handled ++ newCycle // O(n)
        newCycle :: cyclesOf(perm, newReady)
      case None =>
        Nil
    }
}

object FastCycler extends Cycler {
  /** Return the list of disjunct cycles sorted ascending by cycle.head */
  def cyclesOf(perm: Seq[Int]): Cycles =
    cyclesOf(Nil, perm, new Array[Boolean](perm.length), 0)

  @tailrec private
  def cyclesOf(ready: List[Cycle], perm: Seq[Int],
               handled: Array[Boolean], prev: Int): Cycles = // c *
    (prev+1 to perm.length) find (i => !(handled(i-1))) match { // O(i_c) //\sum{i_c} = n
      case Some(start) =>
        val aCycle = cycle(perm, start)
        for (i <- aCycle)
          handled(i-1) = true // O(n_c); Side effects are not harmful, it's tailrec!
        cyclesOf(aCycle :: ready, perm, handled, start)
      case None =>
        ready.reverse // O(n)
    }
  /** Small helper function, finding one cycle. */
  private def cycle(perm: Seq[Int], start: Int): Cycle = {// O(n_c)
    @tailrec def step(ready: List[Int], idx: Int): Cycle = // O(n_c)
      if(start == idx)
        ready.reverse // O(1)
      else
        step(idx :: ready, perm(idx - 1))
    (start :: step(Nil, perm(start - 1)))
  }
}
