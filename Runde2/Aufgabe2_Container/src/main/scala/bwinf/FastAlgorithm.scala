package de.voodle.tim.bwinf.container

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer

object FastAlgorithm {
  def compute(perm: Seq[Int]): Seq[Instruction] = compute(Utils cyclesOf perm)
  def compute(cycles: List[List[Int]]): Seq[Instruction] = computeCycle(cycles.head, cycles.tail)._1

  def computeCycle(cycle: List[Int], other: List[List[Int]]): (Queue[Instruction],List[List[Int]]) =
    if(cycle.tail.isEmpty) (Queue(Take, Put), other) else { // Handle trivial case. (Only one element)
      val max = cycle.max
      //println("Max of cycle " + cycle + " is " + max)
      val erster = cycle.head
      val initial = (Queue(Take) : Queue[Instruction], other, erster)
      val (instrs, cyclesLeft, last) =
	(initial /: (cycle.tail :+ erster)) {
          case ((instrs, cyclesLeft, prev), cur) =>
	    if(prev == max && (!cyclesLeft.isEmpty)) { // "Concat" Cycles
	      //val (prevCycles, afterCycles) = cyclesLeft.partition(_.head < prev) // TODO: Better?.. Well, prevCycles should be Empty
	      val (cycleInstrs, newCyclesLeft) = computeCycle(cyclesLeft.head, cyclesLeft.tail)
	      val extraInstrs = instrs + Put + MoveRight ++ cycleInstrs + MoveLeft + Take
	      step(extraInstrs, /* prevCycles ::: */ newCyclesLeft, prev, cur)
	    } else
	      step(instrs, cyclesLeft, prev, cur)
        }
      // Then move last(where we should be) to first.
      // println("instrs: " + instrs)
      (instrs.init, // drop last PUT
       cyclesLeft)
    }
  @tailrec
  private def step(instrs: Queue[Instruction], cyclesLeft: List[List[Int]], prev: Int, cur: Int): (Queue[Instruction], List[List[Int]], Int) =
    cyclesLeft.headOption match { // Does another Cycle begins between prev and cur?
      case Some(nextCycle @ (next :: _)) if next < cur =>
	// Next Cycle (if nextCycle.head > cur then x.head > cur for all x <- trans.tail)
	val (transInstrs, newTransLeft) = computeCycle(nextCycle, cyclesLeft.tail)
	// Move to nextCycle.head (next), then to cycle, then go on to cur.
	val newInstrs = instrs + Move(prev -> next) + Put ++ transInstrs + Take // + Move(next -> cur) + Put + Take
	step(newInstrs, newTransLeft, next, cur)
      case _ =>
	val newInstrs = instrs + Move(prev -> cur) + Put + Take
	(newInstrs, cyclesLeft, cur)
    }
}