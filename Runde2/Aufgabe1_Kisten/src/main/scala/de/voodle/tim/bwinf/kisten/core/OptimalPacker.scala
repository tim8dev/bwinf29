package de.voodle.tim.bwinf.kisten.core

class OptimalPacker(kistenListe: Seq[KisteLeer]) extends SortierenderPacker(kistenListe)
                                                    with SchrittPacker {
  protected def packSchritt(sätze: Set[KistenSatz], kiste: KisteLeer) =
    if(sätze.isEmpty)
      Set(KistenSatz(kiste :: Nil)) // KistenSatz nur mit der Kiste
    else
      (Set[KistenSatz]() /: sätze) { // Beginne mit leerer Menge
        (menge, satz) => // Füge neue Möglichkeiten der menge hinzu
         menge ++ (satz ++< kiste) // (satz ++< kiste) erzeugt neue Möglichkeiten
      }
}