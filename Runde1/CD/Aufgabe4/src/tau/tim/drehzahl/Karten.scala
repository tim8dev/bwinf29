package tau.tim.drehzahl

import Wuerfel.{zahlen, wahrscheinlichkeit}

/**
 * Ein Kartensatz, z.B. als Handkarten.
 * Definiert Funktionen um
 * die Durchschnittspunktzahl (für eine Zahl, oder allgemein),
 * für eine gegebene Zahl Möglichkeiten für Streichsätze,
 * einen neuen Kartensatz nach Streichung eines anderen
 * zu berechnen.
 */
class Karten(private val karten : List[Int]) extends Ordered[Karten] {
  lazy val punkte = karten.sum
  lazy val gewichtetePunkte: Double = gewichtetePunkteListe.sum

  /**
   * Berechne für jede mögliche Würfelzahl die erwartete Punktzahl,
   * gewichtet nach Wahrscheinlichkeit
   */
  private lazy val gewichtetePunkteListe: Seq[Double] =
    for(zahl <- zahlen)
    yield wahrscheinlichkeit(zahl)*gewichtetePunkte(streichbar(zahl))
  // Rekursive Hilfskuntion zur Punkteberechnung:
  private def gewichtetePunkte(streichbar: List[Karten]): Double =
    // Rekursionsschluss, wenn keine Karte streichbar
    if(streichbar.isEmpty) punkte
    else { // Rekursiver Aufruf:
      // Errechnen der Punktzahl für alle durch Streichen erhaltene Kartensätze
      val ges = for(s <- streichbar) yield streiche(s).gewichtetePunkte
      // Erwartete Punktzahl ist das Maximum
      ges.max
    }

  /**
   * Berechnet die durchschnittlich zu erwartende Punktzahl bei Würfeln der
   * angegebenen Zahl.
   * @param zahl Die Würfelzahl
   */
  def gewichtetePunkte(zahl: Int) : Double = gewichtetePunkteListe(zahl)

  /**
   * Streicht die gegebenen Karten aus diesem Kartensatz und gibt
   * diesen neuen Kartensatz zurück.
   * @param that Die rauszustreichenden Karten
   */
  def streiche(that: Karten): Karten =
    if(that.karten.toSet.subsetOf(this.karten.toSet)) {
      val ret = Karten(karten diff that.karten)
      ret
    } else throw new IllegalArgumentException()

  /**
   * Berechnet für eine gegebene Zahl nach den Spielregeln mögliche Kartensätze
   * um diese Zahl aus dem Kartensatz zu streichen.
   * @param num Die zu streichende Zahl
   */
  def streichbar(num: Int) : List[Karten] = streichbar(num, Nil, karten)

  /**
   * Rekursive Hilfsfunktion zu streichbar
   * @param nums: Die Zahlen sind abnehmend geordnet
   */
  private def streichbar(num: Int, nums: List[Int], karten: List[Int]): List[Karten] =
    if(num < 0) Nil // Abbruch bei negativen Zahlen.
    // Rekursionsschluss, drehe Zahlen für steigende Sortierung
    else if(num == 0) Karten(nums.reverse) :: Nil
    // Rekursionsschluss wenn keine Zahlen mehr vorhanden
    else if(karten.isEmpty) Nil
    else {
      val i = karten.head // Betrag der betrachteten Karte
      // Rekursiver Aufruf, einmal nach streichen (abziehen) des Betrages
      streichbar(num-i, i :: nums, karten.tail) :::
      streichbar(num,        nums, karten.tail) // <- einmal ohne
    }

  // Ordnung nach Punkten:
  def compare(andere : Karten) = punkte - andere.punkte
  override lazy val toString = karten.mkString("[", "|", "]")
  }

object Karten extends Karten(Nil) {
  // Hilfsfunktionen zur einfachen Erzeugung von Kartensätzen
  def apply(karten: Int*) : Karten = apply(karten.toList)
  def apply(karten: List[Int]) =
    new Karten(karten.toList.sortWith((x,y) => x < y))

  // Der Standardkartensatz
  lazy val standard = apply(1,2,3,4,5,6,7,8,9)
}
