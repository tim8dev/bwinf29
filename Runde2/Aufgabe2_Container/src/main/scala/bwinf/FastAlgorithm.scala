package de.voodle.tim.bwinf.container

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer

object FastAlgorithm {
  def compute(perm: Seq[Int]): Seq[Instruction] =
    computeCycles(Utils cyclesOf perm) // O(n)
  def computeCycles(cycles: Seq[Seq[Int]]): Seq[Instruction] =
    Queue(TakeCon) ++ computeCycle(cycles.head, cycles.tail)._1

  /**
   * Should be called, after a TakeCon!
   * When a cycle starts, all the containers in the cycles are supposed to be on the
   * container side.
   * Container are always transported on the Container side!
   */
  private def computeCycle(cycle: Seq[Int], other: Seq[Seq[Int]]): (Queue[Instruction], Seq[Seq[Int]]) = {
    val max = cycle.max
    val erster = cycle.head
    val initial = (Queue() : Queue[Instruction], other, erster)
    val (instrs, cyclesLeft, last) =
      (initial /: (cycle.tail :+ erster)) { // each step: one up @con
        case ((instrs, cyclesLeft, prev), cur) =>
          if(prev == max && (!cyclesLeft.isEmpty) && cyclesLeft.head.head == prev+1) { // "Concat" Cycles
            val (cycleInstrs, newCyclesLeft) = computeCycle(cyclesLeft.head, cyclesLeft.tail)
            val extraInstrs = instrs + PutCon + MoveRight + TakeCon ++ cycleInstrs + MoveLeft + TakeCon
            step(extraInstrs, newCyclesLeft, prev, cur)
          } else
            step(instrs, cyclesLeft, prev, cur)
      }
    (instrs, cyclesLeft)
  }

  @tailrec
  private def step(instrs: Queue[Instruction], cyclesLeft: Seq[Seq[Int]],
                   prev: Int, cur: Int): (Queue[Instruction], Seq[Seq[Int]], Int) =
    cyclesLeft.headOption match { // Does another Cycle begins between prev and cur?
      case Some(nextCycle @ (next :: _)) if next < cur =>
        // Next Cycle (if nextCycle.head > cur then x.head > cur for all x <- trans.tail)
        val (transInstrs, newCyclesLeft) = computeCycle(nextCycle, cyclesLeft.tail)
        // Move to nextCycle.head (next), then to cycle, then go on to cur.
        val newInstrs = instrs + Move(prev -> next) + Rotate + TakeCon + Rotate + PutCon + Rotate ++ transInstrs
        step(newInstrs, newCyclesLeft, next, cur)
      case _ =>
        val newInstrs = instrs + Move(prev -> cur) + Rotate + PutWag + TakeCon
        (newInstrs, cyclesLeft, cur)
    }
}