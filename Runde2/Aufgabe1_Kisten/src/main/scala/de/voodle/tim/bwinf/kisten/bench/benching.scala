package de.voodle.tim.bwinf.kisten
package bench

import core._

import java.util.concurrent.Future
import scala.actors.Actor
import Actor._
import scala.collection.immutable.Stack

private[bench] abstract sealed trait BenchNachricht
private[bench] case object StarteBench extends BenchNachricht
private[bench] case object StoppeBench extends BenchNachricht
private[bench] case class WertUpdate(packer: BenchPacker, wert: BenchWert) extends BenchNachricht

abstract sealed trait BenchWert {
  val time: Long
  val wert: Long
}
case class AussenVolumen(time: Long, wert: Long)     extends BenchWert
case class EinzelVolumen(time: Long, wert: Long)     extends BenchWert
case class AnzahlKistenSätze(time: Long, wert: Long) extends BenchWert

class BenchBucket(val werteSpeichern: Boolean) {
  private var avs = Stack[AussenVolumen]()
  private var evs = Stack[EinzelVolumen]()
  private var aks = Stack[AnzahlKistenSätze]()

  private def updateStack[T <: BenchWert](wert: T, stack: Stack[T]): Stack[T] = {
    if(werteSpeichern)
      if(!stack.isEmpty) {
        val (last, newStack) = stack.pop2
        if(last.wert == wert.wert)
          newStack push wert
        else
          stack push wert
      } else
        stack push wert
    else
      Stack(wert)
  }

  def update(benchWert: BenchWert) = benchWert match {
    case wert: AussenVolumen => avs = updateStack(wert, avs)
    case wert: EinzelVolumen => evs = updateStack(wert, evs)
    case wert: AnzahlKistenSätze => aks = updateStack(wert, aks)
  }

  override def toString = Seq(avs,evs,aks) mkString ("\n", "\n", "\n")
}

class Bencher(werteSpeichern: Boolean, packer: Seq[BenchPacker],
                                       listener: Seq[BenchListener]) extends Actor {
  val buckets = Map(packer map (_ -> new BenchBucket(werteSpeichern)) : _*)

  def act() {
    this ! StarteBench
    loop {
      react {
        case StarteBench =>
          buckets.keySet.foreach(_.start(this))
        case StoppeBench =>
          buckets.keySet.foreach(_.exit)
        case WertUpdate(packer, wert) =>
          update(packer, wert)
      }
    }
  }

  private def update(packer: BenchPacker, wert: BenchWert) = {
    listener.foreach(_.update(packer, wert))
    buckets(packer).update(wert)
  }

  def stop(force: Boolean) =
    if(force) this !? StoppeBench // TODO: Force!
    else this ! StoppeBench // TODO: Wait for termination.

  override def toString = buckets mkString "\n"
}

trait BenchListener {
  def update(packer: BenchPacker, wert: BenchWert): Unit
}

trait BenchPacker extends HilfsPacken {
  def start(bencher: Bencher): Unit
  def exit: Unit
}

object BenchPacker {
  def curTime = System.currentTimeMillis
  def update(bencher: Bencher, packer: BenchPacker,
             aussenVolumen: Long, einzelVolumen: Long, anzahl: Long) {
    val time = curTime
    bencher ! WertUpdate(packer, AussenVolumen(time, aussenVolumen))
    bencher ! WertUpdate(packer, EinzelVolumen(time, einzelVolumen))
    bencher ! WertUpdate(packer, AnzahlKistenSätze(time, anzahl))
  }

  // TODO: Default Bencher!
  
}
