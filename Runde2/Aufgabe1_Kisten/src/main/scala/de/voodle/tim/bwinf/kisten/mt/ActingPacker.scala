package de.voodle.tim.bwinf.kisten
package mt

import core._
import java.util.concurrent.CountDownLatch
import scala.collection.mutable
import scala.actors.Actor
import Actor._

sealed trait PackenderAktor extends Actor

class EmpfangenderAktor(kisten: List[Kiste]) extends PackenderAktor {
  @volatile private var minKs = KistenSatz(kisten)
  @volatile private var empfangen = 0
  private val signal = new CountDownLatch(1)

  def get() = { signal.await; minKs }

  def act() = react {
    case ks: KistenSatz =>
      empfangen += 1
      if(ks.v < minKs.v) minKs = ks
      act()
    case Stop(amt) =>
      if(empfangen == amt) {
        signal.countDown // READY!
        exit()
      } else {
        this ! Stop(amt)
        act()
      }
  }
}

class BerechnenderAktor(val ziel: PackenderAktor, val kiste: Kiste) extends PackenderAktor {
  var gesendet = 0
  var abgearbeitet = 0
  val bereitsGesendet = mutable.HashSet[KistenSatz]()

  def act() = react {
    case ks: KistenSatz =>
      val ms = ks ++< kiste
      for(m <- ms; if(!bereitsGesendet.contains(m))) {
        bereitsGesendet += m
        gesendet += 1
        ziel ! m
      }
      abgearbeitet += 1
      act()
    case Stop(amt) => // Ich bin fertig;
      if(abgearbeitet == amt) {
        println("STOPPED: amt = " + amt + ", karton: " + kiste)
        ziel ! Stop(gesendet)
        bereitsGesendet.clear
        exit() // und beschließe zu gehen
      } else { // Es gibt noch Arbeit - werd nicht hurtig
        this ! Stop(amt) // Merke! - aber bleibe nicht stehen!
        act()
      }
  }
}

class ActingPacker(kistenListe: Seq[KisteLeer]) extends SortierteKisten(kistenListe) {
  private val ziel = new EmpfangenderAktor(kisten)

  private def erzeugeAktoren(i: Int): PackenderAktor = {
    val aktor = if(i >= kisten.length) ziel
                else new BerechnenderAktor(erzeugeAktoren(i+1), kisten(i))
    aktor.start()
    aktor
  }

  private def startePacken() {
    val startAktor = erzeugeAktoren(0)
    startAktor ! KistenSatz(Nil)
    startAktor ! Stop(1)
  }

  override def min: KistenSatz = {
    startePacken()
    ziel.get
  }
}
