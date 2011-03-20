package de.voodle.tim.bwinf.kisten
package bench

import core._
import online._

import scala.actors.Actor
import Actor._


private[bench] abstract sealed trait BenchNachricht
private[bench] case object StarteBench extends BenchNachricht
private[bench] case object StoppeBench extends BenchNachricht
private[bench] case class WertUpdate(packer: BenchPacker, wert: Long) extends BenchNachricht

/*
class BenchBucket {
import scala.collection.mutable.Stack
  private val avs = Stack[AussenVolumen]()
  private val evs = Stack[EinzelVolumen]()
  private val aks = Stack[AnzahlKistenSätze]()
  private val ak = Stack[AnzahlKisten]()

  private def updateStack[T <: BenchWertTyp.Value](wert: T, stack: Stack[T]) {
    if(!stack.isEmpty) {
      val last = stack.pop
      if(last.wert == wert.wert)
        stack push wert
      else
        stack push (last, wert)
    } else
      stack push wert
  }

  def update(benchWert: BenchWert) = benchWert match {
    case wert: AussenVolumen => updateStack(wert, avs)
    case wert: EinzelVolumen => updateStack(wert, evs)
    case wert: AnzahlKistenSätze => updateStack(wert, aks)
    case wert: AnzahlKisten => updateStack(wert, ak)
  }

  override def toString = Seq(avs,evs,aks,ak) mkString ("\n", "\n", "\n")
}*/

class Bencher(werteSpeichern: Boolean, val packers: Seq[BenchPacker],
                                       val listener: Seq[BenchListener]) {
  //val buckets = Map(packer map (_ -> new BenchBucket) : _*)
  private def actorFor(f: (BenchListener) => (BenchPacker, Long) => Unit) =
    actor { loop { react {
      case WertUpdate(packer, wert) => listener.foreach(f(_)(packer, wert))
          }      }       }
  val AussenVolumen = actorFor(_.updateAussenVolumen _)
  val EinzelVolumen = actorFor(_.updateEinzelVolumen _)
  val AnzahlKistenSätze = actorFor(_.updateAnzahlKistenSätze _)
  val AnzahlKisten = actorFor(_.updateAnzahlKisten _)

  def start() {
    actor {
      packers.foreach(_.start(this))
    }
  }

  def stop(force: Boolean) =
    if(force) Actor.exit // TODO: Force!
    else () // TODO: Wait for termination.

  override def toString = packers mkString "\n"
}

object Bencher {
  def defaultOfflineBenchPacker(kisten: Seq[KisteLeer]) = List(
    new OnlineBenchPacker(kisten, OnlineAlgo()),
    new OfflineBenchPacker(kisten, FindeHalbleeren ::
                                   FindeGrößerenLeeren :: Nil) )

  def defaultBencher(kisten: Seq[KisteLeer],
                     listener: BenchListener*): Bencher =
    defaultBencherFromPackers(new OptimalBenchPacker(kisten) ::
                              defaultOfflineBenchPacker(kisten), listener)

  def defaultBencherFromPackers(packer: Seq[BenchPacker],
                              listener: Seq[BenchListener]) =
    new Bencher(false, packer,listener)
}

trait BenchListener {
  def updateAussenVolumen(packer: BenchPacker, wert: Long): Unit
  def updateEinzelVolumen(packer: BenchPacker, wert: Long): Unit
  def updateAnzahlKistenSätze(packer: BenchPacker, wert: Long): Unit
  def updateAnzahlKisten(packer: BenchPacker, wert: Long): Unit
}

trait BenchPacker {
  def start(bencher: Bencher): Unit
  def exit: Unit
}

object BenchPacker {
  //private[bench] def curTime = System.currentTimeMillis
  private[bench] def update(bencher: Bencher, packer: BenchPacker,
             einzelVolumen: Long, aussenVolumen: Long,
             anzahlKistenSätze: Long, anzahlKisten: Long) {
    bencher.EinzelVolumen ! WertUpdate(packer, einzelVolumen)
    bencher.AussenVolumen ! WertUpdate(packer, aussenVolumen)
    bencher.AnzahlKistenSätze ! WertUpdate(packer, anzahlKistenSätze)
    bencher.AnzahlKisten ! WertUpdate(packer, anzahlKisten)
  }
}
