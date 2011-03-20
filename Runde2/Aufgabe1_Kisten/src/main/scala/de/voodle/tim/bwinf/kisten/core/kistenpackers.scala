package de.voodle.tim.bwinf
package kisten
package core

trait KistenPacker {
  val kisten: List[KisteLeer] //= KistenPacker.sortieren(kartonListe) // sortieren; umdrehen
  def min: KistenSatz
}

object SortierteKisten {
  // Sortiert wird von Groß nach Klein!
  // D'rum muss die Ordnung 'falsch'rum sein
  def sortieren(input: Seq[KisteLeer]): List[KisteLeer] =
    input.toList.sorted(Kiste.Ordnung.nachVolumen.reverse)
}

abstract class SortierteKisten(kartonListe: Seq[KisteLeer]) extends KistenPacker {
  val kisten = SortierteKisten.sortieren(kartonListe) // sortieren; umdrehen
}

trait SimplerPacker extends KistenPacker {
  def min = packe.min(KistenSatz.Ordnung.nachVolumen)
  def packe: Traversable[KistenSatz]
}

trait HilfsPacken extends SimplerPacker {
  def packe = (Set[KistenSatz]() /: kisten) ( hilfsPacken )
  protected def hilfsPacken(sätze: Set[KistenSatz], kiste: KisteLeer): Set[KistenSatz]
}

trait GraphPacker extends KistenPacker {
  implicit val kistenGraph = KistenGraph(kisten)
}

/*
import java.util.Random
class RandomPacker(kistenSeq: Seq[KisteLeer]) extends SimplerPacker(kistenSeq) {
  private val rand = new Random
  
  lazy val packe: Set[KistenSatz] =
    kisten.foldLeft(Set[KistenSatz]()) { (gepackt, u) => hilfsPacken(gepackt, u) }

  private var i = 0

  protected def hilfsPacken(sätze: Set[KistenSatz], kiste: Kiste): Set[KistenSatz] =
    if(sätze.isEmpty) Set(KistenSatz(kiste :: Nil))
    else {
      var fertig: Set[KistenSatz] = Set()
      for(satz <- sätze)
        if(i < 8 || rand.nextBoolean) // TODO: More random! (Weighted Random!)
          fertig ++= (satz +< kiste)
      i += 1
      fertig
    }
}
*/