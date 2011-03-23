package de.voodle.tim.bwinf.container

object Putter2 {
  def berechne(transs: List[List[Int]]): List[Instruction] = {
    def berechneZyklus(trans: List[Int], other: List[List[Int]]): List[Instruction] = {
      val erster = trans.head
      val initial = (Take :: List.empty[Instruction], other, erster)
      val (instrs, transLeft, last) = (initial /: trans.tail) {
        case ((instrs, transLeft, prev),cur) =>
	  println("((instrs, transLeft, prev),cur): " + ((instrs, transLeft, prev),cur))
	  transLeft.headOption match { // Beginnt hier eine andere Transp.?
	    case Some(nextTrans @ (head :: _)) if head < cur  =>
	      // Nächste Transposition (wenn trans.head.head < cur dann auch x.head < cur für alle x <- trans.tail) TODO: BEWEIS!
	      val transInstrs = berechneZyklus(nextTrans, transLeft.tail)
	      // Move to nextTrans.head
	      (Take :: Put :: bewege(nextTrans.head -> cur) ::: (Take :: transInstrs) ::: (Put :: bewege(prev -> nextTrans.head)) ::: instrs, transLeft.tail, cur) // TODO: Remove in berechneZyklus handled Transpositions
            case _ => (Take :: Put :: bewege(prev -> cur) ::: instrs, transLeft, cur)
	}
      } // Then move last(where we should be) to first.
      println("instrs: " + instrs)
      Put :: bewege(trans.last -> erster) ::: instrs
    }
    berechneZyklus(transs.head, transs.tail).reverse
  }

  private def bewege(vonNach: (Int,Int)): List[Instruction] =
    vonNach match { // TODO: make this O(1), then overall complexity would be O(n*c) only :)
      case (von,nach) if von < nach => List.fill(nach-von)(MoveRight)
      case (von,nach) if nach < von => List.fill(von-nach)(MoveLeft)
      case _ => Nil
    }
}