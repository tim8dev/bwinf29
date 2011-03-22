package tau.tim.logistik

import java.util.Scanner
import java.util.StringTokenizer
import scala.io.Source

object TourenPlan {
  // ===== Hauptmethoden zur eigentlichen Berechnung: =====
  /**
   * Berechnet aus einer Liste mit je 6 Werten eine mögliche Zahl an Fahrzeugen
   * für jeden Standort A, B, und C.
   * Die Wert in der Liste sind wie in den Beispieldateien:
   * A->B, A->C, B->A, B->C, C->A, C->B
   *
   * @param einAus: Die Liste mit den Werten.
   */
  def berechne(einAus: List[(Int,Int,Int,Int,Int,Int)], log: String => _) = {
    // Aus den Spalten entsprechende Werte rausfischen
    // Berechnet gleichzeitig die Differenz zwischen Ein- und Ausfuhren für jeden Tag.
    val a_diff = for(i <- einAus) yield i._3+i._5 - (i._1+i._2)
    val b_diff = for(i <- einAus) yield i._1+i._6 - (i._3+i._4)
    val c_diff = for(i <- einAus) yield i._2+i._4 - (i._5+i._6)

    log { "Fahrzeugdifferenzen für Standort A: " + a_diff.mkString("\n  ", "\n  ", "") }
    log { "Fahrzeugdifferenzen für Standort B: " + b_diff.mkString("\n  ", "\n  ", "") }
    log { "Fahrzeugdifferenzen für Standort C: " + c_diff.mkString("\n  ", "\n  ", "") }

    // Die Fahrzeugzahlen für jeden Standort:
    val a = 0 :: berechneStandort(0, a_diff, (s:String) => log{ " Standort A: " + s})
    val b = 0 :: berechneStandort(0, b_diff, (s:String) => log{ " Standort B: " + s})
    val c = 0 :: berechneStandort(0, c_diff, (s:String) => log{ " Standort C: " + s})

    // Die Anzahl der Fahrzeuge für jeden Standort ist das Minimum.
    val res = (-a.min, -b.min, -c.min) // .min <= 0, weil 0 am Anfang angehängt wurde.
    log { "Anzahl für Standort A: " + res._1 }
    log { "Anzahl für Standort B: " + res._2 }
    log { "Anzahl für Standort C: " + res._3 }
    res
  }

  private def berechneStandort(vorher : Int, diffs: Seq[Int], log: String => _) : List[Int] =
    diffs.headOption match {
      case Some(diff) =>
        // Der jetzige Wert ist der vorher + die Differenz aus den Ein- und Ausfuhren
        val jetzt = vorher + diff
        log { "aktueller Zwischenwert: " + jetzt }
        jetzt ::
        // Rekursiver Aufruf mit Zwischenwert jetzt und Restwerten diffs.tail
        berechneStandort(jetzt, diffs.tail, log)
      case _ => Nil // wenn keine Werte mehr vorhanden, Rekursionsschluss.
    }

  // ===== Hilfsfunktionen zum einlesen einer Datei, etc. =====
  def ausDatei(tabelle: Source) = leseZeilen(tabelle.getLines.toList)
  def ausString(tabelle: String) = leseZeilen(new StringTokenizer(tabelle, "\n"))

  private def leseZeilen(lines: List[String]) : List[(Int,Int,Int,Int,Int,Int)] = {
    if(lines.isEmpty) Nil
    else {
      leseZeile(lines.head) match{
        case Some(x) => x :: leseZeilen(lines.tail)
        case None    =>      leseZeilen(lines.tail)}
    }
  }

  private def leseZeilen(scanner: Scanner) : List[(Int,Int,Int,Int,Int,Int)] =
    if(scanner.hasNextLine) {
      val zeile = leseZeile(scanner.nextLine) match{
        case Some(x) => List(x)
        case None    => Nil}
      zeile ::: leseZeilen(scanner)
    } else Nil

  private def leseZeilen(tokens : StringTokenizer) : List[(Int,Int,Int,Int,Int,Int)] =
    if(tokens.hasMoreTokens) {
      val zeile = leseZeile(tokens.nextToken) match{
        case Some(x) => List(x)
        case None    => Nil}
      zeile ::: leseZeilen(tokens)
    } else Nil

  private def leseZeile(s : String) : Option[(Int,Int,Int,Int,Int,Int)] =
    if(s.startsWith("#"))
      None
    else {
      val t = new StringTokenizer(s)
      def nextInt : Int = t.nextToken.toInt
      Some((nextInt, nextInt, nextInt, nextInt, nextInt, nextInt))
    }
}
