package de.voodle.tim.bwinf.container

import scala.annotation.tailrec

// TODO: New Instruction set.
class Maschine(initial: Gleis,
               val print: Boolean = false) {
  protected val gleis = initial
  private def digits(num: Int) = (math.log10(num) + 1).floor.toInt
  private val numLength = digits(initial.length)
  private val space = " " * (numLength+1)
  private val arr = "-" * (numLength+1)
  //private var instrs = instructions.toList
  //private var cur = 0
  //private var idx = 1

  def log(str: =>Any) =
    if(print) println(str) else ()

  def interpret(instrs: Seq[Instruction]): Gleis = {
    log(initial.container map {
        con =>
        val diff = numLength - digits(con)
        " " * diff + con
      } mkString (" "))
    log((1 to initial.length) map {
        con =>
        val diff = numLength - digits(con)
        " " * diff + con
      } mkString (" "))
    interpret(instrs.toList,0,0,1)
  }

  protected def act(instrs: List[Instruction]) {}

  @tailrec
  private def interpret(instrs: List[Instruction], con: Int, wag: Int, idx: Int): Gleis = {
    /*if(print) {
     println("Current Instruction: " + (instrs.headOption getOrElse "Nothing"))
     println("Current Item: " + (if(cur == 0) "Nothing" else cur))
     println("Current Index: " + idx)
     println("Current Gleis: \n" + gleis)
     }*/
    act(instrs)
    instrs match { // Recursivly check
      case Rotate :: xs =>
        interpret(xs,wag,con,idx)
      case TakeCon :: xs =>
        interpret(xs, (gleis takeCon idx) getOrElse 0, wag, idx)
      case TakeWag :: xs =>
        interpret(xs, 0, (gleis takeWag idx) getOrElse 0, idx)
      case PutCon :: xs =>
        if(
          gleis putCon (idx -> con) isEmpty
        ) log("WARNING! Con:@" + (idx -> con))
        interpret(xs, 0, wag, idx)
      case PutWag :: xs =>
        if(
          gleis putWag (idx -> wag) isEmpty
        ) log("WARNING! Wag:@" + (idx -> con))
        interpret(xs, con, 0, idx)
      case MoveRight(len) :: xs =>
        log(space * (idx-1) + arr * len + ">")
        if(wag != 0)
          log("Warning: transporting " + wag)
        interpret(xs, con, wag, idx+len)
      case MoveLeft(len) :: xs =>
        log(space * (idx-1-len) + "<" + arr * len)
        if(wag != 0)
          log("Warning: transporting " + wag)
        interpret(xs, con, wag, idx-len)
      case Nil => gleis // Do Nothing
    }
  }
  override def toString = gleis.toString
}
