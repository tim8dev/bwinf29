package de.voodle.tim.bwinf.container

import scala.annotation.tailrec

/**
 * TODO: Semantics!
 * WARNING: This Maschine works different from the Simple one!
 
class KranMaschine(instructions: Seq[Instruction], val kran: KranInterface) {
// NOTE: Container are ALWAYS PUT ON THE WAGGONS!
  private type Kran = KranInterface
  private val kranInstrs: Seq[KranInterface => Unit] = computeKran()
  private var cur = 0
  private var idx = 1

  private def kranAction(action: KranInterface => Boolean => Unit)(left: Boolean): Seq[KranInterface => Unit] = {
    (_.greiferBewegen(left)) :: // down
    (kran => action(kran)(left)) ::
    (_.greiferBewegen(left)) :: // up
    Nil
  }

  @tailrec def computeKran(instrs: Seq[Instruction]) = {
    val simplified = Instruction simplify instrs
    simplified flatMap {
      _ match {
	case Swap =>
	  kranAction(_.takeContainer)(false) ::: // Take the container on the waggons!
	  ((_.rotate()) ::
	  kranAction(_.putContainer)(false))
	case Put =>
	  
      }
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
*/