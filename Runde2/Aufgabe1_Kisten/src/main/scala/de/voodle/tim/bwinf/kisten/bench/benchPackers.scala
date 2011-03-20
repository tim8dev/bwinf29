package de.voodle.tim.bwinf.kisten
package bench

import scala.actors.Actor
import Actor._
import core._
import BenchPacker._
import online._

class OnlineBenchPacker(kistenSeq: Seq[KisteLeer], packer: OnlineAlgo)
                                   extends BenchPacker {
  private val kisten = kistenSeq.toList
  private val onlineStart = packer

  def start(bencher: Bencher) {
    actor {
      (onlineStart /: kisten) (packe(bencher) _)
    }
  }

  def exit {}

  private var fertigeKisten = 0
  private var fertigeKistenVolumen = 0

  private def packe(bencher: Bencher)(online: OnlineAlgo, kiste: KisteLeer): OnlineAlgo = {
    fertigeKisten += 1
    fertigeKistenVolumen += kiste.v
    val res = online + kiste
    BenchPacker.update(bencher, this, fertigeKistenVolumen, res.kisten.v, 1, fertigeKisten)
    res
  }
}

class OfflineBenchPacker(kistenSeq: Seq[KisteLeer], strategien: List[Strategie])
                                    extends OnlinePacker(kistenSeq, strategien)
                                       with BenchPacker {
  private var bencher: Bencher = _

  def start(bencher: Bencher) {
    this.bencher = bencher
    actor {
      this.min
    }
  }

  def exit {}

  private var fertigeKisten = 0
  private var fertigeKistenVolumen = 0
  
  override def min = (onlinePacker /: kisten) {
    (oPacker, kiste) =>
      fertigeKisten += 1
      fertigeKistenVolumen += kiste.v
      val newOPacker = oPacker + kiste
      BenchPacker.update(bencher, this, fertigeKistenVolumen,
                         newOPacker.kisten.v, 1, fertigeKisten)
      newOPacker
    }.kisten

}

class OptimalBenchPacker(kistenSeq: Seq[KisteLeer]) extends OptimalPacker(kistenSeq) with BenchPacker {
  private var bencher: Bencher = _

  def start(bencher: Bencher) {
    this.bencher = bencher
    actor {
      this.min
    }
  }

  def exit {}

  private var fertigeKisten = 0
  private var fertigeKistenVolumen = 0

  override protected def packSchritt(sätze: Set[KistenSatz], kiste: KisteLeer) = {
    fertigeKistenVolumen += kiste.v
    fertigeKisten += 1
    if(sätze.isEmpty) {
      val ks = KistenSatz(kiste :: Nil)
      update(bencher, this, kiste.v, kiste.v, 1, fertigeKisten)
      Set(ks)
    } else {
      var minKSVolumen = Int.MaxValue
      (Set[KistenSatz]() /: sätze) {
        (menge, satz) =>
          val neueSätze = (satz ++< kiste)
          val ergebnis = menge ++ neueSätze
          val neuesMin = neueSätze.min(KistenSatz.Ordnung.nachVolumen).v
          if(neuesMin < minKSVolumen)
            minKSVolumen = neuesMin
          update(bencher, this,
                 fertigeKistenVolumen, minKSVolumen,
                 math.max(sätze.size, ergebnis.size),
                 fertigeKisten)
          ergebnis
      }
    }
  }
}

