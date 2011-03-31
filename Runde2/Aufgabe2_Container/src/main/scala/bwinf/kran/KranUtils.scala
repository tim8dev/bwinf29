package de.voodle.tim.bwinf.container

import mci.KranInterface
import kran._

object MCIUtils {
  def createKran(permLength: Int, kran: KranInterface) = {
    val perm = Utils randPerm permLength
    val instrs = FastAlgorithm compute perm
    val kranBuff = new KranInstrsBuffer
    val kranMaschine = new KranMaschine(perm, kranBuff)
    kranMaschine interpret instrs
    kranBuff.createInstructions(kran)
  }
}