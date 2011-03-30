package de.voodle.tim.bwinf.container

import scala.annotation.tailrec

class SimpleMaschine(instructions: Seq[Instruction], initial: Gleis, val print: Boolean = false) {
  private val gleis = initial
  private var instrs = instructions.toList
  private var cur = 0
  private var idx = 1
  //def set(instrs: Seq[Instruction]): Gleis = set(instrs.toList, 0, 1)
  
  def next {
    if(print) {
      println("Current Instruction: " + instrs)
      println("Current Item: " + (if(cur == 0) "Nothing" else cur))
      println("Current Index: " + idx)
      println("Current Gleis: \n" + gleis)
    }
    def swap(xs: List[Instruction]) = { // Helper function
      val swap = (gleis take idx) getOrElse 0
      gleis put (idx -> cur)
      swap
    }
    instrs match { // Recursivly check
      case Swap :: xs =>
        set(xs, swap(xs), idx)
      case Put :: Take :: xs =>
        set(xs, swap(xs), idx)
      case Take :: Put :: xs =>
	set(xs, swap(xs), idx)
      case Take :: xs =>
	set(xs, (gleis take idx) getOrElse 0, idx)
      case Put :: xs =>
	gleis put (idx -> cur)
	set(xs, 0, idx)
      case MoveRight(len) :: xs =>
	set(xs, cur, idx+len)
      case MoveLeft(len) :: xs =>
	set(xs, cur, idx-len)
      case Nil => gleis // Do Nothing
    }
  }

  private def set(instrs: List[Instruction], cur: Int, idx: Int) = {
    this.instrs = instrs
    this.cur = cur
    this.idx = idx
  }
  override def toString = gleis.toString
}
