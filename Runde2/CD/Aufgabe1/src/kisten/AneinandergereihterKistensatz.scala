package de.voodle.tim.bwinf.kisten

class AneinandergereihterKistensatz(val kistensätze: List[Kistensatz])
  extends Kistensatz(
            kistensätze.head.kistenBaum,
            kistensätze.head.v,
            kistensätze.head.length) {
  override def neben(der: Kistensatz) = {
    new AneinandergereihterKistensatz(der :: kistensätze)
  }

  // DON't use other methods than overriden ones!

  override val v      = kistensätze map (_.v) sum
  override val length = kistensätze map (_.length) sum
  override def kistenSet = kistensätze map (_.kistenSet) reduceLeft (_ ++ _)
  override def kisten = kistensätze map (_.kisten) reduceLeft (_ ++ _) sorted
  override def toStream = kistensätze map (_.toStream) reduceLeft (_ append _)
}
