package de.voodle.tim.bwinf
package kisten
package core

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeMap

case class KistenSatz (kistenBaum: TreeMap[Kiste, Int],
                                v: Int,
                           length: Int)
  extends Ordered[KistenSatz] {
  import KistenSatz._

  def find(f: Kiste => Boolean): List[Kiste] =
    kistenBaum.find(k => f(k._1)) map (_._1 :: Nil) getOrElse // Gibt es Wurzel?
    (((None: Option[List[Kiste]]) /: kistenSet) { // Sonst suche in Elementen.
      (vorher, kiste) =>
        if(vorher.isEmpty) {
          val pfad = kiste.finde(f)
          if(pfad.isEmpty)
            None
          else
            Some(pfad)
        } else vorher
    } getOrElse Nil)

  def -(der: Kiste) = KistenSatz(subFromTree(kistenBaum, der), v-der.v, length-1)
  def +(der: Kiste) = KistenSatz(  addToTree(kistenBaum, der), v+der.v, length+1)
  
  def +<(der: Kiste)(implicit vergleich: KistenVergleich = StandardVergleich): Set[KistenSatz] =
    (Set[KistenSatz]() /: kistenBaum) {
        (saetze, ki) => {
          val k = ki._1
          val treeOhneK = subFromTree(kistenBaum, k)
          val gepackt = (k +< der)(vergleich)
          val ms = gepackt.map(g => KistenSatz(addToTree(treeOhneK, g), v, length)) // k.v == g.v
          saetze ++ ms
        }
      }
  def ++<(der: Kiste)(implicit vergleich: KistenVergleich = StandardVergleich): Set[KistenSatz] =
    (this +< der)(vergleich) + (this + der)

  def compare(der: KistenSatz) = KistenSatz.Ordnung.eindeutig.compare(this, der)

  override def equals(other: Any) =
    other.isInstanceOf[KistenSatz] && equals(other.asInstanceOf[KistenSatz])
  def equals(der: KistenSatz) = // Schneller HashCode basierter Check
    hashCode == der.hashCode && kistenBaum == der.kistenBaum

  override val hashCode = 41* (43 /: kistenBaum) { 47 *(_) + _.hashCode } + v // Speicher im Eifer!

  def kistenSet: SortedSet[Kiste] = SortedSet(kistenBaum.map(_._1).toSeq: _*)
  def kisten = kistenBaum.flatMap((ki) => ki._1 * ki._2).toSeq.sorted
  def toStream = kisten.flatMap(_.toStream).toStream
  override def toString = kisten.mkString("{\n", ",\n", "\n}")
}

object KistenSatz {
  def addToTree(baum: TreeMap[Kiste, Int], karton: Kiste) =
    baum.updated(karton, baum.getOrElse(karton, 0) + 1)
  
  def subFromTree(baum: TreeMap[Kiste, Int], karton: Kiste) =
    baum.get(karton) match {
      case Some(i) => if(i > 1) baum.updated(karton, i-1) else (baum - karton)
      case _ => baum
    }
  private def createTree(tree: TreeMap[Kiste, Int], liste: Seq[Kiste]): TreeMap[Kiste, Int] =
    if(liste.isEmpty) tree
    else createTree(addToTree(tree, liste.head), liste.tail)

  def apply(kartons: Seq[Kiste]): KistenSatz =
      apply(createTree(TreeMap.empty, kartons))
  def apply(kistenBaum: TreeMap[Kiste, Int]): KistenSatz =
    KistenSatz(kistenBaum,
               (0 /: kistenBaum) { (v,ki) => v + ki._1.v * ki._2 },
               (0 /: kistenBaum) { (n,ki) => n + ki._2 } )

  object Ordnung {
    val nachVolumen = new Ordering[KistenSatz]() {
      def compare(x: KistenSatz, y: KistenSatz) = x.v compare y.v
    }
    val eindeutig = new Ordering[KistenSatz]() {
      def compare(x: KistenSatz, y: KistenSatz) =
        if(x.v != y.v) x.v - y.v
        else compare(x.kisten, y.kisten)

      private def compare(x: Iterable[Kiste], y: Iterable[Kiste]): Int =
        if(x.isEmpty && y.isEmpty) 0
        else if(x.isEmpty) -1
        else if(y.isEmpty) 1
        else {
          val diff = x.head compare y.head
          if(diff == 0) compare(x.tail, y.tail)
          else diff
        }
    }
  }
}
