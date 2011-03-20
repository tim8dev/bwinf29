package de.voodle.tim.bwinf.kisten.core
package online

class OfflinePacker(kistenSeq: Seq[KisteLeer]) extends SortierteKisten(kistenSeq)
                                               with HilfsPacken {
  var onlinePacker = OnlinePacker()
  protected def hilfsPacken(sätze: Set[KistenSatz], kiste: KisteLeer) = {
    onlinePacker += kiste
    Set()
  }
}

case class OnlinePacker(kisten: KistenSatz, kistenGraph: KistenGraph, strategien: List[Strategie]) {
  def + (der: KisteLeer): OnlinePacker = {
    val neuerKistenGraph = kistenGraph + der           // O(n)
    //val groessere = neuerKistenGraph findeGroessere der // O(n)
    //val kleinere  = neuerKistenGraph findeKleiner  der // O(n)
    val moeglicheKisten = (Option.empty[KistenSatz] /: strategien) {
      (vorher, strategie) => // entweder vorher, oder, diese strategie benutzen!
       vorher orElse strategie(kisten)(der)(neuerKistenGraph)
    }
    // sonst aneinanderreihen, wie einfach :D
    val neueKisten = moeglicheKisten getOrElse ( kisten + der )
    OnlinePacker(neueKisten, neuerKistenGraph, strategien)
  }
}

object OnlinePacker { // Hilfsmethoden zum schnellen erzeugen! :)
  def apply(): OnlinePacker = apply(KistenSatz(Nil))
  def apply(kartons: KistenSatz): OnlinePacker = apply(kartons,
      FindeGroesserenLeeren :: // (2)
      FindeZwischenraum     :: // (3)
      FindeHalbleeren       :: // (1)
      FindeKleinereWurzel   :: // (4)
      Nil)
  def apply(kartons: KistenSatz, strategien: List[Strategie]): OnlinePacker =
    OnlinePacker(kartons,
                 KistenGraph(kartons.kisten.map{ _. alsLeer }), strategien)
}
