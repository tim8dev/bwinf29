package de.voodle.tim.bwinf.container

import scala.annotation.tailrec
import scala.collection.mutable.tim.ListBuffer

object Instructor {
  def compute(perm: Seq[Int], cycler: Cycler = FastCycler): Seq[Instruction] =
    computeFromCycles(cycler cyclesOf perm) // O(n)
  def computeFromCycles(cycles: List[List[Int]]): List[Instruction] =
    TakeCon :: computeCycle(cycles.head, cycles.tail)._1.toList

  /**
   * Should be called, after a TakeCon!
   * When a cycle starts, all the containers in the cycles are supposed to be on the
   * container side.
   * Container are always transported on the Container side!
   */
  private def computeCycle(cycle: List[Int], other: List[List[Int]]): (ListBuffer[Instruction], List[List[Int]]) = {
    val max = cycle.max // O(n_c)
    val erster = cycle.head
    val initial = (ListBuffer[Instruction](), other, erster)
    val (instrs, cyclesLeft, last) =
      (initial /: (cycle.tail :+ erster)) { // each step: one up @con
        case ((instrs, cyclesLeft, prev), cur) =>
          if(prev == max && (!cyclesLeft.isEmpty) && cyclesLeft.head.head == prev+1) { // "Concat" Cycles
            val (cycleInstrs, newCyclesLeft) =
              computeCycle(cyclesLeft.head, cyclesLeft.tail)
            val extraInstrs = instrs ++=
              ListBuffer(PutCon, MoveRight, TakeCon) ++=
              cycleInstrs ++= ListBuffer(MoveLeft, TakeCon)
            step(extraInstrs, newCyclesLeft, prev, cur)
          } else
            step(instrs, cyclesLeft, prev, cur)
      }
    (instrs, cyclesLeft)
  }

  @tailrec
  private def step(instrs: ListBuffer[Instruction], cyclesLeft: List[List[Int]],
                   prev: Int, cur: Int): (ListBuffer[Instruction], List[List[Int]], Int) =
    cyclesLeft.headOption match { // Does another Cycle begins between prev and cur?
      case Some(nextCycle @ (next :: _)) if next < cur =>
        val (transInstrs, newCyclesLeft) = computeCycle(nextCycle, cyclesLeft.tail)
        // Move from prev to nextCycle.head (next)
        val newInstrs = instrs ++=
          ListBuffer(Move(prev -> next), Rotate, TakeCon, Rotate, PutCon, Rotate) ++=
          transInstrs
        step(newInstrs, newCyclesLeft, next, cur)
      case _ =>
        val newInstrs = instrs ++= ListBuffer(Move(prev -> cur), Rotate, PutWag, TakeCon)
        (newInstrs, cyclesLeft, cur)
    }
}