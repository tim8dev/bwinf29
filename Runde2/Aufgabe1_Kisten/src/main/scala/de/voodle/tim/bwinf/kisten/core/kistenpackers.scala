package de.voodle.tim.bwinf
package kisten
package core

trait KistenPacker {
  val kisten: List[KisteLeer] //= KistenPacker.sortieren(kartonListe) // sortieren; umdrehen
  def min: KistenSatz
}

object SortierenderPacker {
  // Sortiert wird von Groß nach Klein!
  // D'rum muss die Ordnung 'falsch'rum sein
  def sortieren(input: Seq[KisteLeer]): List[KisteLeer] =
    input.toList.sorted(Kiste.Ordnung.nachVolumen.reverse)
}

abstract class SortierenderPacker(kistenListe: Seq[KisteLeer]) extends KistenPacker {
  val kisten = SortierenderPacker.sortieren(kistenListe) // sortieren; umdrehen
}

trait SimplerPacker extends KistenPacker {
  def min = packe.min(KistenSatz.Ordnung.nachVolumen)
  def packe: Traversable[KistenSatz]
}

trait SchrittPacker extends SimplerPacker {
  def packe = (Set[KistenSatz]() /: kisten) ( packSchritt )
  protected def packSchritt(sätze: Set[KistenSatz], kiste: KisteLeer): Set[KistenSatz]
}
