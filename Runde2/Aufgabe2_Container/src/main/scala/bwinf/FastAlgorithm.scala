package de.voodle.tim.bwinf.container

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer

object FastAlgorithm {
  def compute(perm: Seq[Int]): Seq[Instruction] = computeCycles(Utils cyclesOf perm) // O(n)
  def computeCycles(cycles: Seq[Seq[Int]]): Seq[Instruction] = computeCycle(cycles.head, cycles.tail)._1

  /**
   * When a cycle starts, all the containers in the cycles are supposed to be on the
   * container side.
   * Container are always transported on the Container side!
   */
  private def computeCycle(cycle: Seq[Int], other: Seq[Seq[Int]]): (Queue[Instruction],Seq[Seq[Int]]) = {
    if(cycle.tail.isEmpty && //) { // Handle trivial case. (Only one element)
      //if(
       other.isEmpty)
        (Queue(Move(0), Take, Put), Seq())
      //else
        //computeCycle(other.head, other.tail) match {
          //case (instrs, remain) =>
        //(Queue(Move(0), Take, Swap, Put), other)
        //}
    //}
  else {
    val max = cycle.max
    val erster = cycle.head
    val initial = (Queue(TakeCon) : Queue[Instruction], other, erster)
    val (instrs, cyclesLeft, last) =
      (initial /: (cycle.tail :+ erster)) {
        case ((instrs, cyclesLeft, prev), cur) =>
          if(prev == max && (!cyclesLeft.isEmpty)) { // "Concat" Cycles
            val (cycleInstrs, newCyclesLeft) = computeCycle(cyclesLeft.head, cyclesLeft.tail)
            val extraInstrs = instrs + PutCon + MoveRight ++ cycleInstrs + MoveLeft + TakeCon
            step(extraInstrs, /* prevCycles ::: */ newCyclesLeft, prev, cur)
          } else
            step(instrs, cyclesLeft, prev, cur)
      }
    // drop last Swap, replace it with Put
    (instrs.init + Put, cyclesLeft)
  }}

  @tailrec
  private def step(instrs: Queue[Instruction], cyclesLeft: Seq[Seq[Int]],
                   prev: Int, cur: Int): (Queue[Instruction], Seq[Seq[Int]], Int) =
    cyclesLeft.headOption match { // Does another Cycle begins between prev and cur?
      case Some(nextCycle @ (next :: _)) if next < cur =>
        // Next Cycle (if nextCycle.head > cur then x.head > cur for all x <- trans.tail)
        val (transInstrs, newTransLeft) = computeCycle(nextCycle, cyclesLeft.tail)
        // Move to nextCycle.head (next), then to cycle, then go on to cur.
        val newInstrs = instrs + Move(prev -> next) + Rotate + TakeCon + Rotate + PutCon + Rotate ++ transInstrs + TakeCon // + Move(next -> cur) + Put + Take
        step(newInstrs, newTransLeft, next, cur)
      case _ =>
        val newInstrs = instrs + Move(prev -> cur) + Rotate + PutWag + TakeCon
        (newInstrs, cyclesLeft, cur)
    }
}