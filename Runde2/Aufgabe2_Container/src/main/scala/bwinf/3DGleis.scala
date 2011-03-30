package de.voodle.tim.bwinf.container

import mci.KranInterface

/**
 * Responsible for calling put/take on KranInterface;
 * Also responsible for moving the Kran; // TODO: Better Design!
 * TODO: REMOVE HACKING CODE!
class KranGleis(initCon: Seq[Int], kran: KranInterface) extends SimpleGleis(initCon) {
  // NOTE: Current Container should always be 'left' on the waggons' side
  //         after any (trans)action completed
  // NOTE: right === containers' side
  // NOTE: left === waggons' side
  //private var hasCur = false
  private var idx = 1
  private def kranAction(action: KranInterface => Boolean => Unit)(left: Boolean) {
    kran.greiferBewegen(left) // down
    action(kran)(left)
    kran.greiferBewegen(left) // up
  }
  override def take(i: Int) = {
    for(_ <- idx to i) kran.move(true) // Move left.
    for(_ <- i to idx) kran.move(false) // Move right.
    idx = i
    arrGet(con, true)(i) match {
      case res @ Some(_) =>
	kranAction(_.takeContainer)(false)
	kran.rotate() // Rotate over to left
	//hasCur = true
	res
      case None =>
	arrGet(wag, true)(i) match {
	  case res @ Some(_) =>
	    kranAction(_.takeContainer)(true)
	    //hasCur = true
	    res
	  case None =>
	    None
	}
    }
  }
  override def put(i: Int)(what: Int) = {
    for(_ <- idx to i) kran.move(true) // Move left.
    for(_ <- i to idx) kran.move(false) // Move right.
    arrPut(wag)(i)(what) match {
      case res @ Some(_) =>
	kran.rotate() // Rotate over to left
	kranAction(_.putContainer)(true)
	//hasCur = false
	res
      case None =>
	arrPut(con)(i)(what) match {
	  case res @ Some(_) =>
	    kranAction(_.putContainer)(false)
	    //hasCur = false
	    res
	  case None =>
	    None
	}
    }
  }
}

 */