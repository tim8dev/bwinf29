package de.voodle.tim.bwinf.kisten

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeMap

case class Kistensatz (kistenBaum: TreeMap[Kiste, Int], v: Int, length: Int)
  extends Ordered[Kistensatz] {
  import Kistensatz._

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

  def -(der: Kiste) = Kistensatz(subFromTree(kistenBaum, der), v-der.v, length-1)
  def +(der: Kiste) = Kistensatz(  addToTree(kistenBaum, der), v+der.v, length+1)
  
  def +<(der: Kiste): Set[Kistensatz] =
    (Set[Kistensatz]() /: kistenBaum) {
        case (saetze, (k, _)) => {
          val treeOhneK = subFromTree(kistenBaum, k)
          val gepackt = (k +< der)
          val ms = gepackt.map(g => Kistensatz(addToTree(treeOhneK, g), v, length)) // k.v == g.v
          saetze ++ ms
        }
      }
  def ++<(der: Kiste): Set[Kistensatz] =
    (this +< der) + (this + der)

  def neben(der: Kistensatz) =
    new AneinandergereihterKistensatz(der :: this :: Nil)

  def compare(der: Kistensatz) = Kistensatz.Ordnung.eindeutig.compare(this, der)

  override def equals(other: Any) =
    other.isInstanceOf[Kistensatz] && equals(other.asInstanceOf[Kistensatz])
  def equals(der: Kistensatz) = // Schneller HashCode basierter Check
    hashCode == der.hashCode && kistenBaum == der.kistenBaum

  override val hashCode = 41* (43 /: kistenBaum) { 47* _ + _.hashCode } + v // Speicher im Eifer!

  def kistenSet: SortedSet[Kiste] = SortedSet(kistenBaum.map(_._1).toSeq: _*)
  def kisten = kistenBaum.flatMap((ki) => ki._1 * ki._2).toSeq.sorted
  def toStream = kisten.flatMap(_.toStream).toStream
  override def toString = kisten.mkString("{\n", ",\n", "\n}")
}

object Kistensatz {
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

  def apply(kisten: Seq[Kiste]): Kistensatz =
      apply(createTree(TreeMap.empty, kisten))
  def apply(kistenBaum: TreeMap[Kiste, Int]): Kistensatz =
    Kistensatz(kistenBaum,
              (0 /: kistenBaum) { (v,ki) => v + ki._1.v * ki._2 },
              (0 /: kistenBaum) { (n,ki) => n + ki._2 } )

  object Ordnung {
    val nachVolumen = new Ordering[Kistensatz]() {
      def compare(x: Kistensatz, y: Kistensatz) = x.v compare y.v
    }
    val eindeutig = new Ordering[Kistensatz]() {
      def compare(x: Kistensatz, y: Kistensatz) =
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
