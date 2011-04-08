package de.voodle.tim.bwinf.container

import mci.KranInterface
import kran._

object MCIUtils {
  def createKran(permLength: Int, kran: KranInterface) = {
    val perm = Utils randPerm permLength
    val instrs = Instructor compute perm
    val kranBuff = new KranInstrsBuffer
    val kranMaschine = new KranMaschine(new Gleis(perm), kranBuff)
    kranMaschine interpret instrs
    kranBuff.createInstructions(kran)
  }
}