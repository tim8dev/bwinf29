package de.voodle.tim.bwinf.container

import scala.collection.immutable.Queue

object FastAlgorithm {
  def compute(perm: Seq[Int]): Seq[Instruction] = compute(Utils cyclesOf perm)
  def compute(cycles: List[List[Int]]): Seq[Instruction] = {
    val (instrs, remainingCycles) = computeCycle(cycles.head, cycles.tail)
    if(remainingCycles.isEmpty)
      instrs
    else { // TODO: 'Concat'
      instrs //+ Move+ compute(remainingCycles)
    }
  }

  def computeCycle(cycle: List[Int], other: List[List[Int]]): (Queue[Instruction],List[List[Int]]) = {
    val max = cycle.max
    if(cycle.tail.isEmpty) (Queue(Take, Put), other) else {
      val erster = cycle.head
      val initial = (Queue(Take) : Queue[Instruction], other, erster)
      val (instrs, cyclesLeft, last) =
	(initial /: cycle.tail) {
           case ((instrs, cyclesLeft, prev), cur) => step(instrs,cyclesLeft,prev,cur)
        }
      // Then move last(where we should be) to first.
      // println("instrs: " + instrs)
      (instrs + Move(cycle.last -> erster) + Put, cyclesLeft)
    }
  }
  def step(instrs: Queue[Instruction], cyclesLeft: List[List[Int]], prev: Int, cur: Int): (Queue[Instruction], List[List[Int]], Int) =
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
/*
  private def bewege(vonNach: (Int,Int)): List[Instruction] =
    vonNach match { // TODO: make this O(1), then overall complexity would be O(n*c) only :)
      case (von,nach) if von < nach => List.fill(nach-von)(MoveRight)
      case (von,nach) if nach < von => List.fill(von-nach)(MoveLeft)
      case _ => Nil
    }*/
}