package de.voodle.tim.bwinf.container
import annotation.tailrec

class Maschine(protected val gleis: Gleis,
                 private val print: Boolean = false) {
  import Maschine._
  private val length = gleis.length
  private val numLength = digits(length)
  private val space = " " * (numLength+1)
  private val arrow = "-" * (numLength+1)

  private def minLength =
    gleis.container.zipWithIndex.map { case (v,i) => BigInt(((i+1)-v).abs) } sum

  def log(str: =>Any)(implicit forcePrint: Option[String] = None) = forcePrint match {
      case Some(force) if print => println(str + force)
      case Some(force) => println(force)
      case None if print => println(str)
      case _ => ()
    }
  
  def logInts(ints: =>Seq[Int]): String =
    (for(i <- ints) yield {
        val diff = numLength - digits(i)
        " " * diff + i
      }) mkString (" ")

  def interpret(instrs: Seq[Instruction]): Gleis = {
    log(logInts(1 to length) + ";")(Some("(m=" + minLength + ")"))
    log(logInts(gleis.container) + ";")(Some("(l=" + instrs.map(i => BigInt(i.len)).sum + ")"))
    interpret(instrs.toList,0,0,1)
  }
  // Attach point for further actions (for subclasses)
  protected def act(instrs: Option[Instruction]) {}
  
  @tailrec
  private def interpret(instrs: List[Instruction], con: Int, wag: Int, idx: Int): Gleis = {
    act(instrs.headOption)
    instrs match { // Recursivly check
      case Rotate :: xs =>
        interpret(xs,wag,con,idx)
      case TakeCon :: xs =>
        interpret(xs, gleis takeCon idx, wag, idx)
      case TakeWag :: xs =>
        interpret(xs, 0, gleis takeWag idx, idx)
      case PutCon :: xs =>
        gleis putCon (idx -> con)
        interpret(xs, 0, wag, idx)
      case PutWag :: xs =>
        gleis putWag (idx -> wag)
        interpret(xs, con, 0, idx)
      case MoveRight(len) :: xs =>
        log(space * (idx-1) + arrow * len + ">" +
            space * (length-len-idx) + " (" + len + ")")
        interpret(xs, con, wag, idx+len)
      case MoveLeft(len) :: xs =>
        log(space * (idx-1-len) + "<" + arrow * len +
            space * (length-idx) + " (" + len + ")")
        interpret(xs, con, wag, idx-len)
      case Nil => gleis // Do Nothing
    }
  }
  override def toString = gleis.toString
}
object Maschine {
  private def digits(num: Int) = (math.log10(num) + 1).floor.toInt
}