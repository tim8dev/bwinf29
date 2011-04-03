package de.voodle.tim.bwinf.container
package kran

class KranMaschine(initial: KranGleis, private val kranBuff: KranInstrsBuffer) extends Maschine(initial, true) {
  def this(perm: Seq[Int], kranBuff: KranInstrsBuffer) =
    this(new KranGleis(perm, kranBuff), kranBuff)

  import KranGleis._
  
  private var cur = 0
  private var idx = 1

  override def interpret(instrs: Seq[Instruction]) =
    super.interpret(//Instruction simplifyPath
                    instrs.toList)

  override def act(instrs: List[Instruction]) =
    instrs match {
      case MoveRight(len) :: xs =>
        for(_ <- 1 to len) kranBuff.add(_.move(false))
      case MoveLeft(len) :: xs =>
        for(_ <- 1 to len) kranBuff.add(_.move(true))
      case _ => () // Do Nothing special
    }
  
  override def toString = gleis.toString
}

