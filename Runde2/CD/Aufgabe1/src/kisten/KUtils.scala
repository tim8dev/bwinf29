package de.voodle.tim.bwinf.kisten

import java.math.RoundingMode.{UP => ROUND_UP}
import java.text.DecimalFormat

object KUtils {
  import StandardKisten._ // Standardkartongrößen
  import KistenLogger._
  import LogStufe._
  import java.math.MathContext

  private val logger = KistenLogger(print, str => ())
  import logger.logge

  private implicit val mathContext = new MathContext(4, ROUND_UP)

  def bench(packer: Kistenpacker, kistenFunktion: ()=>Seq[KisteLeer],
            wiederholungen: Int, verbose: Boolean = true): (BigDecimal, Long) = {
    logge("Packer: " + packer)
    var ergebnisse: List[((BigInt, BigInt), Long)] = Nil
    var i = 0
    while(i < wiederholungen) {
      val kisten = kistenFunktion()

      val (min, zeit) = bench(packer min kisten)
      logge("Minimum: " + min)(verbose)

      val v0 = kisten.map(i => BigInt(i.v)).sum
      val v1 = min.kisten.map(i => BigInt(i.v)) reduceLeft (_ + _)
      ergebnisse ::= ((v0,v1), zeit)
      i += 1
    }
    val gesamtVolumen = ergebnisse.map(_._1._1).sum
    val endVolumen    = ergebnisse.map(_._1._2) reduceLeft (_ + _)
    val komprimierung = BigDecimal(
      BigDecimal(gesamtVolumen).bigDecimal
        .divide(BigDecimal(endVolumen).bigDecimal, mathContext)
      )
    val zeiten = ergebnisse map (_._2)
    val zeit = zeiten.sum / zeiten.length
    val format = new DecimalFormat("##.##")
    logge("Wiederholungen\tKomprimierungsgrad\tZeit")
    logge(wiederholungen + "\t\t" + format.format(komprimierung) + "\t\t\t" + format.format(zeit))
    (komprimierung, zeit)
  }

  private def zufallsKisten(n: Int) = List.fill(n)(zufall)

  // Hilfsmethode zum benchen.
  private def bench[T](packer: =>T): (T, Long) = {
    val startZeit = System.currentTimeMillis
    val kistenSaetze = packer
    val endZeit = System.currentTimeMillis
    (kistenSaetze, endZeit - startZeit)
  }
}
