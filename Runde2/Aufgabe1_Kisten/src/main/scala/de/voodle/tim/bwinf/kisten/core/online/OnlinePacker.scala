package de.voodle.tim.bwinf.kisten.core
package online

/** Gliedert den OnlineAlgo in die Packer Hierarchie */
class OnlinePacker(kistenSeq: Seq[KisteLeer], val onlinePacker: OnlineAlgo)
                                         extends SortierenderPacker(kistenSeq) {
  def this(kistenSeq: Seq[KisteLeer], strategien: List[Strategie]) =
    this(kistenSeq, OnlineAlgo(strategien))
  def this(kistenSeq: Seq[KisteLeer]) =
    this(kistenSeq, List(FindeGrößerenLeeren, FindeZwischenraum))

  override def min = (onlinePacker ++ kistenSeq).kisten
}
case class OnlineAlgo(kisten: KistenSatz, strategien: List[Strategie]) {
  /** Einfach alle nacheinander hineinfügen! */
  def ++ (die: Seq[KisteLeer]) = (this /: die)(_ + _)

  def + (der: KisteLeer): OnlineAlgo = {
    val neueKisten = (Option.empty[KistenSatz] /: strategien) {
        // wenn es  vorher gibt, dann vorher, sonst diese strategie benutzen!
         (vorher, strat) => vorher orElse strat(kisten)(der)
      } getOrElse (kisten + der) // sonst aneinanderreihen, wie einfach :D
    OnlineAlgo(neueKisten, strategien)
  }
}
object OnlineAlgo { // Hilfsmethoden zum schnellen erzeugen! :)
  def apply(): OnlineAlgo = apply(KistenSatz(Nil))
  def apply(strategien: List[Strategie]): OnlineAlgo =
    OnlineAlgo(KistenSatz(Nil), strategien)
  def apply(kistenSatz: KistenSatz): OnlineAlgo =
    OnlineAlgo(kistenSatz, standardStrategien)

  def standardStrategien =
    FindeGrößerenLeeren :: // (2)
    FindeZwischenraum   :: // (3)
    FindeHalbleeren     :: // (1)
    FindeKleinereWurzel :: // (4)
    Nil
}
