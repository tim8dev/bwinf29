package de.voodle.tim.bwinf
package kisten
package main

import core._
import online._
import mt._

import java.text.DecimalFormat

object KistenPacken {
  import StandardKisten._ // Standardkartongrößen
  import KistenLogger._
  import LogStufe._

  val logger = KistenLogger(println, getNextFileWriter)
  import logger.logge

  def main(args: Array[String]) { // Der Haupteintrittspunkt des Programmes
    logge("!! ! Berechnung Startet ! !!")

    //testOnline(zufallsKisten(800))
    
    val ergebnisse = for(i <- 1 to 300) yield {
      val e = ((0, 0.0, 0.0) /: (0 until 2)) { // Prüfe 5x und erstelle Durchschnitt!
        (prev, _) =>
        val (count, komp, time) = prev
        val res = testOnline(zufallsKisten(i))
        (count + 1, (komp*count+res._1)/(count+1), (time*count+res._2)/(count+1))
      }
      (i, e._2, e._3)
    }
    val format = new DecimalFormat("##.##")
    logge(ergebnisse.map(e => e._1 + "\t" + format.format(e._2) + "\t" + format.format(e._3)) mkString "\n")(Ergebnis)
    
    //testSt()
    //testWithGraph()
  }

  private def zufallsKisten(n: Int) = List.fill(n)(zufall)

  private val kisten: List[KisteLeer] = {
    import Raum._ // Implicits importieren für schöne Syntax
    (umzugskarton  *3 ::: // 2
     buecherkarton *1 :::
     kleiderbox    *1 :::
     pizzakarton   *2 ::: // 2
     arthur        *2 ::: // <-- x
     fluschi       *1 :::
     christoph     *2 ::: // 2
     paroli        *1 :::
     postmeister   *1 :::
     chachacha     *1)// Die Kartons
  }

  // Hilfsmethode zum benchen.
  private def bench[T](packer: =>T): (T, Long) = {
    val startZeit = System.currentTimeMillis
    val kistenSaetze = packer
    val endZeit = System.currentTimeMillis
    (kistenSaetze, endZeit - startZeit)
  }

  def testOnline(kisten: List[KisteLeer]): (Double, Long) = {
    val v = kisten.map(_.v).sum
    //val sortKisten = kisten.sorted(Kiste.Ordnung.nachVolumen).reverse
    val head = kisten.take(1)
    val (online, time) = bench {
      var online = OnlinePacker(KistenSatz(head))
      for(kart <- kisten.tail) online += kart
      online
    }

    //logge("Ks[Online]: " + online.kisten)
    logge("Ungepackte Kisten- (anzahl, volumen): " + (kisten.length, v))
    logge("Online.v: " + online.kisten.v)
    val komprimierung = (v.toDouble / online.kisten.v.toDouble)
    logge("Komprimierung (um,auf): " + (komprimierung, (online.kisten.v.toDouble / v.toDouble)))
    logge("Mit Online gepackt in " + time + "ms.")
    (komprimierung, time)
  }

  def testWithGraph() {
    val graphPacker = new MutableOptimalPacker(kisten) // mit Graph!
    val (minKs, time) = bench(graphPacker.min)
    logge("Mit Graph gepackt in " + time + " ms." )
    logge("Minimum[Graph].v: " + minKs.v)

    logge("Minimum[Graph]" + minKs)
  }

  def testMt() {
    val (minKs, time) = bench(new ActingPacker(kisten).min)
    logge("Multithreaded gepackt in " + time + " ms.")
    logge("Minimum[mt].v : " + minKs.v)
  }

  def testSt() {
    val (kistenSaetze, time) = bench(new OptimalPacker(kisten).packe)
    logge("Singlethreaded gepackt in " + time + " ms.")
    logge("Anzahl berechneter Möglichkeiten (von " + kisten.size + " Kartons): "
            + kistenSaetze.size)

    val minKs = kistenSaetze.min(KistenSatz.Ordnung.nachVolumen)
    logge("Minimum[st]: " + minKs)
    logge("Minimum[st].v: " + minKs.v)
  }
/*
  def testRandom(times: Int) {
    var timesBenched: List[Long] = Nil
    var volumes: List[Int] = Nil
    val max = KistenSatz(kisten)
    var absMin = max
    for(i <- 0 until times) {
      val packer = new RandomPacker(kisten)
      val (kistenSaetze, time) = bench(packer.packe)
      logge("  RandomKartonPacker gepackt in " + time + " ms.")
      logge("  Berechnete Möglichkeiten[Random] (von " + kisten.size + " Kartons): "
              + kistenSaetze.size)
      
      val minKs = if(kistenSaetze.isEmpty) max else kistenSaetze.min
      logge("  Minimum[Random].v : " + minKs.v)

      if(minKs.v < absMin.v) absMin = minKs
      timesBenched ::= time
      volumes ::= minKs.v
    }

    logge("Durchschnittszeit: " + timesBenched.sum / timesBenched.length)
    logge("Durchschnittsvolumen: " + volumes.sum / volumes.length)
    logge("Minimum[Overall Random]: " + absMin + "(v = " + absMin.v + ")")
  }
*/
}
