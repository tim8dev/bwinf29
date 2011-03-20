package de.voodle.tim.bwinf.kisten
package bench

import scala.actors.Actor
import Actor._
import core._
import BenchPacker._

class OptimalBenchPacker(kistenSeq: Seq[KisteLeer]) extends OptimalPacker(kistenSeq) with BenchPacker {
  var bencher: Bencher = _

  def start(bencher: Bencher) {
    this.bencher = bencher
    actor {
      this.min
    }
  }

  def exit {}

  var fertigesKistenVolumen = 0

  override protected def hilfsPacken(sätze: Set[KistenSatz], kiste: KisteLeer) = {
    fertigesKistenVolumen += kiste.v
    if(sätze.isEmpty) {
      val ks = KistenSatz(kiste :: Nil)
      update(bencher, this, kiste.v, kiste.v, 1)
      Set(ks)
    } else {
      var minVolumen = Int.MaxValue
      (Set[KistenSatz]() /: sätze) {
        (menge, satz) =>
        val ergebnis = menge ++ (satz ++< kiste)
        val neuesMin = ergebnis.min(KistenSatz.Ordnung.nachVolumen).v
        if(neuesMin < minVolumen)
          minVolumen = neuesMin
        update(bencher, this, minVolumen, fertigesKistenVolumen, ergebnis.size)
        ergebnis
      }
    }
  }
}

