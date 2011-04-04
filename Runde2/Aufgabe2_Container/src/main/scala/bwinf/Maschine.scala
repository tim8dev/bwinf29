package de.voodle.tim.bwinf.container

import scala.annotation.tailrec

// TODO: New Instruction set.
class Maschine(protected val gleis: Gleis,
               private val print: Boolean = false) {
  private def digits(num: Int) = (math.log10(num) + 1).floor.toInt
  private val numLength = digits(gleis.length)
  private val space = " " * (numLength+1)
  private val arr = "-" * (numLength+1)

  def log(str: =>Any) = if(print) println(str) else ()

  def logInts(ints: =>Seq[Int]): Unit =
    log(ints map {
        con =>
        val diff = numLength - digits(con)
        " " * diff + con
      } mkString (" "))

  def interpret(instrs: Seq[Instruction]): Gleis = {
    logInts(1 to gleis.length)
    logInts(gleis.container)
    interpret(instrs.toList,0,0,1)
  }

  protected def act(instrs: List[Instruction]) {}

  @tailrec
  private def interpret(instrs: List[Instruction], con: Int, wag: Int, idx: Int): Gleis = {
    act(instrs)
    instrs match { // Recursivly check
      case Rotate :: xs =>
        interpret(xs,wag,con,idx)
      case TakeCon :: xs =>
        interpret(xs, (gleis takeCon idx) getOrElse 0, wag, idx)
      case TakeWag :: xs =>
        interpret(xs, 0, (gleis takeWag idx) getOrElse 0, idx)
      case PutCon :: xs =>
        gleis putCon (idx -> con)
        interpret(xs, 0, wag, idx)
      case PutWag :: xs =>
        gleis putWag (idx -> wag)
        interpret(xs, con, 0, idx)
      case MoveRight(len) :: xs =>
        log(space * (idx-1) + arr * len + ">")
        interpret(xs, con, wag, idx+len)
      case MoveLeft(len) :: xs =>
        log(space * (idx-1-len) + "<" + arr * len)
        interpret(xs, con, wag, idx-len)
      case Nil => gleis // Do Nothing
    }
  }
  override def toString = gleis.toString
}
