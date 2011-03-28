package de.voodle.tim.bwinf.container

import scala.annotation.tailrec

object Instruction {
  def simplifyPath(instrs: List[Instruction]) = {
    @tailrec
    def simplify(ready: List[Instruction], instrs: List[Instruction]): List[Instruction] =
      instrs match {
	case Put :: Take :: xs =>
	  simplify(ready, Swap :: xs)
	case Take :: Put :: xs =>
	  simplify(ready, Swap :: xs)
	case MoveRight(len1) :: MoveRight(len2) :: xs =>
	  simplify(ready, MoveRight(len1 + len2) :: xs)
	case other :: xs =>
	  simplify(other :: ready, xs)
	case Nil =>
	  ready
      }

    simplify(Nil, instrs).reverse
  }
}

sealed trait Instruction {
  type Path = Seq[Instruction]
  def len: Int = 0
  def short: String = "" + toString.head
}
sealed trait Move extends Instruction {
  override def len = 1
  override def short = (toString filter (_.isUpper)) + "(" + len + ")"
}
object Move {
  def apply(len: Int): Move =
    if(len > 0) MoveRight(len)
    else        MoveLeft(-len)
  def apply(fromTo: (Int, Int)): Move = {
    val (from,to) = fromTo
    Move(to - from)
  }
  def unapply(move: Move): Int = move match {
    case MoveRight(len) => len
    case MoveLeft(len) => -len
  }
}
case object Swap extends Instruction
case object Take extends Instruction
case object Put  extends Instruction
object MoveLeft extends MoveLeft(1)
case class MoveLeft(override val len: Int) extends Move
object MoveRight extends MoveRight(1)
case class MoveRight(override val len: Int) extends Move