package de.voodle.tim.bwinf.container

object Putter {
  def put(perm: Seq[Int]) = {
    var instrs: List[Instruction] = Nil
    val transs = Transposition trans perm
    val len = perm.length
    var transStarts = transs map (_.head) drop 1 toSet // Drop first, cause it's handled right away.
    val gleis: Gleis = new Gleis(perm)
    var idx = 1
    var cur: Int = (gleis take 1).get
    instrs ::= Take
    println("(idx,cur,transStarts)" + (idx,cur,transStarts))
    if(cur > idx) {
      idx += 1
      instrs ::= MoveRight
    }
    while(idx > 0) {
      println("(idx,cur,transStarts)" + (idx,cur,transStarts))
      cur = // Find new Current.
	if(cur == 0) { // Does this occur?
	  val res = gleis.take(idx) getOrElse 0
	  if(res != 0) instrs ::= Take
	  res
	} else if(idx == cur || (transStarts contains idx)) {
	  transStarts -= idx // Remove new Trans (if exists)
	  gleis.take(idx) match {
	    case Some(nCur) =>
	      instrs ::= Take
	      gleis.put(idx)(cur)
	      instrs ::= Put
	      nCur
	    case None =>
	      gleis.put(idx)(cur)
	      instrs ::= Put
	      0 // 0 indicating no cur.
	  }
	} else // just Skip.
	  cur
      // Now go left or right, depending on what
      if(cur > idx) {
	idx += 1
	instrs ::= MoveRight
      } else if(cur < idx) {
	if(transStarts.isEmpty || idx == len) {
          idx -= 1
	  instrs ::= MoveLeft
	} else { // TODO: Better Integration
	  gleis.put(idx)(cur)
          instrs ::= Put
	  idx += 1
          instrs ::= MoveRight
	  cur = gleis.take(idx) getOrElse 0
	  if(cur != 0) instrs ::= Take
	  transStarts -= idx
	}
      }
    }
    (gleis, (instrs drop 1).reverse) // Drop last MoveLeft!
  }
  def length(instrs: List[Instruction]) = instrs map (_.len) sum
}