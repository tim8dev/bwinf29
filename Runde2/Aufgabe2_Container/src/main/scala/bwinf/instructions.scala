package de.voodle.tim.bwinf.container

sealed trait Instruction {
  type Path = Seq[Instruction]
  def len: Int = 0
  def short: String = "" + toString.head
}
sealed trait Move extends Instruction {
  override def len = 1
  override def short = toString filter (_.isUpper)
}
case object Take extends Instruction
case object Put  extends Instruction
case object MoveLeft extends Move
case object MoveRight extends Move