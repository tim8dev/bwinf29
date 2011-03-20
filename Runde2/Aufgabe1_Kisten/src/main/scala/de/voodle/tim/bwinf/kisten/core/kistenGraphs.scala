package de.voodle.tim.bwinf
package kisten
package core

import scala.collection.immutable.{SortedSet, TreeSet}

object KistenGraph {
  implicit val kartonOrder = new Ordering[KisteLeer] {
    // (x,y) extra vertausch! Sortierung von groß -> klein
    def compare(y: KisteLeer, x: KisteLeer) = x compare y
  }
  implicit val tupleOrder = new Ordering[(KisteLeer, KisteLeer)] {
    // (x,y) extra vertausch! Sortierung von groß -> klein
    def compare(x: (KisteLeer, KisteLeer), y: (KisteLeer, KisteLeer)) = {
      val diff_1 = kartonOrder.compare(x._1, y._1)
      if(diff_1 != 0) diff_1
      else kartonOrder.compare(x._2, y._2)
    }
  }

  def apply(kartons: Seq[KisteLeer]) = {
    def compute(rels: Set[(KisteLeer, KisteLeer)], rest: SortedSet[KisteLeer]): Set[(KisteLeer, KisteLeer)] =
      if(rest.isEmpty) rels
      else {
        var relsTemp = rels
        val karton = rest.head
        for(der <- rest.tail)
          if(karton ⊃ der)             // Karton muss >=~ dem and'rem sein
            relsTemp += (karton -> der)// Wenn ja dann füg' das Paar hier ein
            //else if(der.innen >=~ karton.aussen)
            //    relsTemp += (der -> karton)
        compute(relsTemp, rest.tail) // Tail-rec
      } // Sort is not good!
    val kartonSet = TreeSet(kartons: _*) // Baum erstellen: O(n log n)
    new KistenGraph(kartonSet, compute(SortedSet(), kartonSet)) // compute: O(n²)
    // Gesamt Komplexität: O(n²); genauer n(n+1)/2..
  }
}

case class KistenGraph(kisten: SortedSet[KisteLeer],
                       kanten: Set[(KisteLeer, KisteLeer)]) extends KistenVergleich {
  def fitIn(kiste: Kiste, in: Kiste): Boolean = fitIn(kiste.alsLeer, in.alsLeer)
  def fitIn(kl: KisteLeer, in: KisteLeer) = kanten.contains(in -> kl) // is in >=~ karton?

  def -<: (kiste: Kiste) = kiste ?<: this
  def ->: (kiste: Kiste) = kiste ?>: this
  def ?<: (kiste: Kiste) = kanten.exists(_._2 == kiste.alsLeer) // "passt in einen?"
  def ?>: (kiste: Kiste) = kanten.exists(_._1 == kiste.alsLeer) // "gibt Karton der reinpasst?"

  def findeGroessere(karton: Kiste) = kanten.filter(_._2 == karton.alsLeer).map(_._1)
    //{ (k) => if(k._2 == karton.alsLeer) Some(k._1) else None }
  def findeKleiner (karton: Kiste) = kanten.filter(_._1 == karton.alsLeer).map(_._2)
  //{ (k) => if(k._1 == karton.alsLeer) Some(k._2) else None }

  def +(kiste: Kiste): KistenGraph = this + kiste.alsLeer
  def +(kl: KisteLeer) =
    KistenGraph(kisten + kl, kanten ++ kisten.flatMap { k =>
        val erstes = if(kl ⊃ k) (kl, k) :: Nil else Nil
        if(k ⊃ kl) (k, kl) :: erstes else erstes
    })
  def -(kiste: Kiste): KistenGraph = this - kiste.alsLeer
  def -(kl: KisteLeer) = KistenGraph(kisten - kl, kanten.filterNot(k => k._1 == kl || k._2 == kl))

  def --(zuStreichen: Traversable[Kiste]) = (this /: zuStreichen) { _ - _ }
  def ++(hinzufuegen: Traversable[Kiste]) = (this /: hinzufuegen) { _ + _ }
}
