package de.voodle.tim.bwinf.container
package kran

//import kran.KranInstructions

/**
 * Responsible for calling put/take on KranInterface;
 * Also responsible for moving the Kran; // TODO: Better Design!
 * TODO(*): REMOVE HACKING CODE!
  // NOTE: Current Container should always be 'left' on the waggons' side
  //         after any (trans)action completed
  // NOTE: right === containers' side
  // NOTE: left === waggons' side
 */
class KranGleis(initCon: Seq[Int], kranBuff: KranInstrsBuffer) extends Gleis(initCon) {
  import KranGleis._ // import from companion
  
  override def take(i: Int) = {
    arrGet(con, true)(i) match {
      case res @ Some(_) =>
        kranAction(_.takeContainer)(false) foreach (kranBuff.add(_))
        kranBuff.add(_.rotate) // Rotate over to left
        res
      case None =>
        arrGet(wag, true)(i) match {
          case res @ Some(_) =>
            kranAction(_.takeContainer)(true) foreach (kranBuff.add(_))
            res
          case None =>
            None
        }
    }
  }
  override def put(i: Int)(what: Int) = {
    arrPut(wag)(i)(what) match {
      case res @ Some(_) =>
        kranAction(_.putContainer)(true) foreach (kranBuff.add(_))
        //kranBuff.add(_.rotate) // Rotate back to right?
        res
      case None =>
        arrPut(con)(i)(what) match {
          case res @ Some(_) =>
            kranBuff.add(_.rotate) // Rotate over to left
            kranAction(_.putContainer)(false) foreach (kranBuff.add(_))
            res
          case None =>
            None
        }
    }
  }
}

object KranGleis {
  private type Kran = mci.KranInterface
  private def kranAction(action: Kran => (Boolean => Unit))(left: Boolean): Seq[Kran => Unit] = {
    ((_: Kran).greiferBewegen(left)) :: // down
    ((kran: Kran) => action(kran)(left)) ::
    ((_: Kran).greiferBewegen(left)) :: // up
    Nil
  }
}