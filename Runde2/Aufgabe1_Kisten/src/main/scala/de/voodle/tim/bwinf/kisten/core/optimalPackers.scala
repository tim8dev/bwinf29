package de.voodle.tim.bwinf.kisten.core

class OptimalPacker(kistenListe: Seq[KisteLeer]) extends SortierteKisten(kistenListe)
                                                    with HilfsPacken {
  protected def hilfsPacken(sätze: Set[KistenSatz], kiste: KisteLeer) =
    if(sätze.isEmpty)
      Set(KistenSatz(kiste :: Nil)) // KistenSatz nur mit der Kiste
    else
      (Set[KistenSatz]() /: sätze) { // Beginne mit leerer Menge
        (menge, satz) => // Füge neue Möglichkeiten der menge hinzu
         menge ++ (satz ++< kiste) // (satz ++< kiste) erzeugt neue Möglichkeiten
      }
}

class MutableOptimalPacker(kistenListe: Seq[KisteLeer]) extends SortierteKisten(kistenListe)
                                                           with SimplerPacker {
  import scala.collection.mutable.HashSet
  override def packe = kisten.foldLeft(HashSet[KistenSatz]()) ( hilfsPacken )
  protected def hilfsPacken(sätze: HashSet[KistenSatz], kiste: KisteLeer): HashSet[KistenSatz] =
    if(sätze.isEmpty) HashSet(KistenSatz(kiste :: Nil))
    else (HashSet[KistenSatz]() /: sätze) {
      (ms, satz) => ms ++= (satz ++< kiste); ms
    }
}

class TreeOptimalPacker(kistenListe: Seq[KisteLeer]) extends SortierteKisten(kistenListe)
                                                        with HilfsPacken {
  import scala.collection.immutable.TreeSet
  protected def hilfsPacken(sätze: Set[KistenSatz], kiste: KisteLeer): TreeSet[KistenSatz] =
    if(sätze.isEmpty) TreeSet(KistenSatz(kiste :: Nil))
    else
      (TreeSet[KistenSatz]() /: sätze) ((ms, satz) => ms ++ (satz ++< kiste))
}

class OptimalGraphPacker(kistenListe: Seq[KisteLeer])
                                         extends SortierteKisten(kistenListe)
                                           with HilfsPacken
                                           with GraphPacker {
  protected def hilfsPacken(sätze: Set[KistenSatz], kiste: KisteLeer): Set[KistenSatz] =
    if(sätze.isEmpty) Set(KistenSatz(kiste :: Nil))
    else {
      (Set[KistenSatz]() /: sätze) {
        (menge, satz) =>
         println("Fertig: amt = " + menge.size + ", Zusatzkiste: " + kiste)
         menge ++ (satz ++< kiste)
      }
    }
}