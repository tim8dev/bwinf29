package de.voodle.tim.bwinf.container
package kran

class KranMaschine(initial: Gleis,
                   private val kranBuff: KranInstrsBuffer,
                   print: Boolean = false) extends Maschine(initial, print) {

  override def act(instrs: List[Instruction]) =
    instrs match {
      // TODO: Implement.
      case MoveRight(len) :: xs =>
        for(_ <- 1 to len) kranBuff.add(_.move(false))
      case MoveLeft(len) :: xs =>
        for(_ <- 1 to len) kranBuff.add(_.move(true))
      case _ => () // Do Nothing special
    }
  
  override def toString = gleis.toString
}
