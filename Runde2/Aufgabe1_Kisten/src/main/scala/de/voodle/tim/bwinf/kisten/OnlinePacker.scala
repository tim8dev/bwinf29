package de.voodle.tim.bwinf.kisten

/** Gliedert den OnlineAlgo in die Packer Hierarchie */
case class OfflinePacker(onlinePacker: OnlineAlgo, sortieren: Boolean)
  extends Kistenpacker {
  def min(kistenSeq: Seq[KisteLeer]) = {
    val kisten = if(sortieren) sortiere(kistenSeq) else kistenSeq
    (onlinePacker ++ kisten).kisten
  }
}
object OfflinePacker {
  def apply(strategien: List[Strategie], sortieren: Boolean = true): OfflinePacker =
    OfflinePacker(OnlineAlgo(strategien), sortieren)
  def apply(sortieren: Boolean): OfflinePacker =
    OfflinePacker(OnlineAlgo.standardStrategien, sortieren)
  def apply(): OfflinePacker =
    OfflinePacker(OnlineAlgo.standardStrategien)
}
case class OnlineAlgo(kisten: Kistensatz, strategien: List[Strategie]) {
  /** Einfach alle nacheinander hineinfügen! */
  def ++ (die: Seq[KisteLeer]) = (this /: die)(_ + _)

  def + (der: KisteLeer): OnlineAlgo = {
    val neueKisten = (Option.empty[Kistensatz] /: strategien) {
        // wenn es  vorher gibt, dann vorher, sonst diese strategie benutzen!
         (vorher, strat) => vorher orElse strat(kisten)(der)
      } getOrElse (kisten + der) // sonst aneinanderreihen, wie einfach :D
    OnlineAlgo(neueKisten, strategien)
  }
}
object OnlineAlgo { // Hilfsmethoden zum schnellen erzeugen! :)
  def apply(): OnlineAlgo = apply(Kistensatz(Nil))
  def apply(strategien: List[Strategie]): OnlineAlgo =
    OnlineAlgo(Kistensatz(Nil), strategien)
  def apply(kistenSatz: Kistensatz): OnlineAlgo =
    OnlineAlgo(kistenSatz, standardStrategien)

  def standardStrategien =
    FindeGrößerenLeeren :: // (2)
    FindeZwischenraum   :: // (3)
    FindeHalbleeren     :: // (1)
    FindeKleinereWurzel :: // (4)
    Nil
}
