package tau.tim.drehzahl

import java.util.Random

// Abstrakter Wuerfel
trait Wuerfel {
  def wahrscheinlichkeit(zahl: Int) : Double =
    haeufigkeit(zahl)/gesamtHaeufigkeit
  //== Abstrakte Funktionen:
  def wuerfeln() : Int
  def gesamtHaeufigkeit : Double
  def haeufigkeit(zahl: Int) : Int
  def zahlen : List[Int]
}

/** Einfacher Wuerfel */
class EinfachWuerfel(private val seiten : Int = 6) extends Wuerfel {
  def wuerfeln() : Int = Wuerfel.rand.nextInt(seiten) +1
  val gesamtHaeufigkeit : Double = seiten
  def haeufigkeit(zahl: Int) = 1
  lazy val zahlen = (1 until seiten+1).toList
}

/** Aus Zwei [gleichen] Wuerfeln zusammengesetzter DoppelWuerfel: */
class DoppelWuerfel(private val wuerfel : Wuerfel) extends Wuerfel {
  private[this] lazy val moeglichkeiten =
    for(x <- wuerfel.zahlen; y <- wuerfel.zahlen) yield x+y
  lazy val zahlen = moeglichkeiten.toSet.toList
  
  def wuerfeln() = wuerfel.wuerfeln() + wuerfel.wuerfeln()
  lazy val gesamtHaeufigkeit : Double = wuerfel.gesamtHaeufigkeit * wuerfel.gesamtHaeufigkeit
  def haeufigkeit(zahl: Int) = moeglichkeiten.count(_==zahl)
}

// Standard Würfel:
object wuerfel6 extends EinfachWuerfel(6)
object doppelWuerfel6 extends DoppelWuerfel(wuerfel6)

/** Wuerfel Objekt mit BasisFunktionen */
object Wuerfel extends DoppelWuerfel(wuerfel6) {
  protected[drehzahl] val rand = new Random()

  def apply(seiten : Int) = new EinfachWuerfel(seiten)

}
