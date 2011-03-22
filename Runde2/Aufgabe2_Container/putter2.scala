object Putter2 {
  def handle //(gleis: Gleis)
    (transs: List[List[Int]]): List[Instruction] = {
    def handleT(trans: List[Int], other: List[List[Int]]): List[Instruction] = {
      val first = trans.head
      val (instrs, transLeft, last) = ((Take :: List.empty[Instruction], other, first) /: trans.tail) {
        case ((instrs, transLeft, prev),cur) =>
	  println("((instrs, transLeft, prev),cur): " + ((instrs, transLeft, prev),cur))
	  transLeft.headOption match { // Beginnt hier eine andere Transp.?
	    case Some(nextTrans @ (first :: _)) if first < cur  =>  // Nächste Transposition (es gibt keine trans mit trans.head < cur) TODO: BEWEIS!
	      val transInstrs = handleT(nextTrans, transLeft.tail)
	      // Move to nextTrans.head
	      (Take :: Put :: moveTo(nextTrans.head -> cur) ::: (Take :: transInstrs) ::: (Put :: moveTo(prev -> nextTrans.head)) ::: instrs, transLeft.tail, cur) // TODO: Remove in handleT handled Transpositions
            case _ => (Take :: Put :: moveTo(prev -> cur) ::: instrs, transLeft, cur)
	}
      } // Then move last(where we should be) to first.
      println("instrs: " + instrs)
      Put :: moveTo(trans.last -> trans.first) ::: instrs
    }
    handleT(transs.head, transs.tail).reverse
  }

  private def moveTo(coords: (Int,Int)): List[Instruction] = { // TODO: make this O(1), then overall complexity would be O(n*c) only :)
    val (from, to) = coords;
    println("(from,to,from-to)" + (from,to,from-to))
    if(from < to) List.fill(to-from)(MoveRight)
    else if(from > to) List.fill(from-to)(MoveLeft)
    else Nil
  }
}