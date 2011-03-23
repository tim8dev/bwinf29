package de.voodle.tim.bwinf.container

trait Maschine {
  def interpret(instrs: Seq[Instruction]): Gleis
}

class SimpleMaschine(initial: Gleis) {
  private val gleis = initial
  final def interpret(instrs: List[Instruction], cur: Int = 0, idx: Int = 1): Unit = {
    def swap(xs: List[Instruction]) {
      val swap = (gleis take idx) getOrElse 0
      gleis put (idx -> cur)
      interpret(xs, swap, idx)
    }
    println("Current Item: " + (if(cur == 0) "Nothing" else cur))
    println("Current Index: " + idx)
    println("Current Gleis: \n" + gleis)
    instrs match { // Recursivly check
      case Put :: Take :: xs => swap(xs)
      case Take :: Put :: xs => swap(xs)
      case Take :: xs =>
	interpret(xs, (gleis take idx) getOrElse 0, idx)
      case Put :: xs =>
	gleis put (idx -> cur)
	interpret(xs, 0, idx)
      case MoveRight(len) :: xs =>
	interpret(xs, cur, idx+len)
      case MoveLeft(len) :: xs =>
	interpret(xs, cur, idx-len)
      case Nil => () // Do Nothing
    }
   }
  override def toString = gleis.toString
}
