package de.voodle.tim.bwinf.container

import scala.annotation.tailrec

trait Maschine {
  def interpret(instrs: Seq[Instruction]): Gleis
}

class SimpleMaschine(initial: Gleis) extends Maschine {
  private val gleis = initial
  def interpret(instrs: Seq[Instruction]): Gleis = interpret(instrs.toList, 0, 1)
  @tailrec
  final def interpret(instrs: List[Instruction], cur: Int, idx: Int): Gleis = {
    println("Current Item: " + (if(cur == 0) "Nothing" else cur))
    println("Current Index: " + idx)
    println("Current Gleis: \n" + gleis)
    def swap(xs: List[Instruction]) = { // Helper function
      val swap = (gleis take idx) getOrElse 0
      gleis put (idx -> cur)
      swap
    }
    instrs match { // Recursivly check
      case Swap :: xs =>
        interpret(xs, swap(xs), idx)
      case Put :: Take :: xs =>
        interpret(xs, swap(xs), idx)
      case Take :: Put :: xs =>
	interpret(xs, swap(xs), idx)
      case Take :: xs =>
	interpret(xs, (gleis take idx) getOrElse 0, idx)
      case Put :: xs =>
	gleis put (idx -> cur)
	interpret(xs, 0, idx)
      case MoveRight(len) :: xs =>
	interpret(xs, cur, idx+len)
      case MoveLeft(len) :: xs =>
	interpret(xs, cur, idx-len)
      case Nil => gleis // Do Nothing
    }
   }
  override def toString = gleis.toString
}
