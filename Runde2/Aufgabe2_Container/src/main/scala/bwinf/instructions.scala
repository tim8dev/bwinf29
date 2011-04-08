package de.voodle.tim.bwinf.container

sealed trait Instruction {
  type Path = Seq[Instruction]
  def len: Int = 0
  def short: String = "" + toString.head
}
case object PutWag extends Instruction
case object PutCon extends Instruction
case object Rotate extends Instruction
case object TakeWag extends Instruction
case object TakeCon extends Instruction
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
case class MoveLeft(override val len: Int) extends Move
case class MoveRight(override val len: Int) extends Move

object MoveLeft extends MoveLeft(1)
object MoveRight extends MoveRight(1)