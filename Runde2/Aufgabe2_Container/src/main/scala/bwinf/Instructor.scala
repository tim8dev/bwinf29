package de.voodle.tim.bwinf.container

import scala.annotation.tailrec
import scala.collection.mutable.tim.ListBuffer

import Cycler._ // import types.

object Instructor {
  def compute(perm: Seq[Int], cycler: Cycler = FastCycler): Seq[Instruction] =
    computeFromCycles(cycler cyclesOf perm)
  def computeFromCycles(cycles: Cycles): Seq[Instruction] =
    TakeCon :: computeCycle(cycles.head, cycles.tail)._1.toList

  /**
   * Should be called, after a TakeCon!
   * When a cycle starts, all the containers in the cycles are supposed to be on the
   * container side.
   * Container are always transported on the Container side!
   */
  private def computeCycle(cycle: Cycle, other: Cycles): (ListBuffer[Instruction], Cycles) = {
    val max = cycle.max // O(n_c)

    type Step = (ListBuffer[Instruction], Cycles, Int)
    @tailrec def step(instrs: ListBuffer[Instruction], cyclesLeft: Cycles,
                      prev: Int, cur: Int): Step =
      cyclesLeft.headOption match { // Does another Cycle begins between prev and cur?
        case Some(nextCycle @ (next :: _)) if prev == max && max+1 == next => // (1)
          val (cycleInstrs, newCyclesLeft) =
            computeCycle(cyclesLeft.head, cyclesLeft.tail)
          val extraInstrs = instrs ++=
            ListBuffer(PutCon, MoveRight, TakeCon) ++=
            cycleInstrs ++= ListBuffer(MoveLeft, TakeCon)
          step(extraInstrs, newCyclesLeft, prev, cur)
        case Some(nextCycle @ (next :: _)) if next < cur => // (2)
          val (transInstrs, newCyclesLeft) = computeCycle(nextCycle, cyclesLeft.tail)
          // Move from prev to nextCycle.head (next)
          val newInstrs = instrs ++=
            ListBuffer(Move(prev -> next), Rotate, TakeCon, Rotate, PutCon, Rotate) ++=
            transInstrs
          step(newInstrs, newCyclesLeft, next, cur)
        case _ => // (3)
          val newInstrs = instrs ++= ListBuffer(Move(prev -> cur), Rotate, PutWag, TakeCon)
          (newInstrs, cyclesLeft, cur)
      }

    val erster = cycle.head
    val initial = (ListBuffer[Instruction](), other, erster)
    val (instrs, cyclesLeft, last) = (initial /: (cycle.tail :+ erster)) {
        case ((instrs, cyclesLeft, prev), cur) =>
            step(instrs, cyclesLeft, prev, cur)
      }
    (instrs, cyclesLeft)
  }
}
