package de.voodle.tim.bwinf
package kisten
package main

import core._
import Kiste._ // Implicits importieren für schöne Syntax

object StandardKisten { // Alle Angaben in cm
  // Quelle: http://umzugskarton.de
  val umzugskarton  = 63 x 31 x  33
  val buecherkarton = 41 x 32 x  34
  val kleiderbox    = 50 x 60 x 135

  // Quelle: http://www.kartonfritze.de
  val pizzakarton = 24 x 24 x 4// .. auch wenn Frau Y. nicht Informatikfreak[in] ist..
  val arthur    = 46 x 38 x 29
  val dinodrei  = 43 x 35 x 10
  val fluschi   = 43 x 34 x 23
  val christoph = 42 x 33 x 37
  val paroli      =  61 x 34 x 35 // "Ideale Kartongröße für Vertreter des weiblichen Geschlechts." laut Kartonfritze
  val postmeister = 120 x 60 x 60 // Was machen jetzt Postmeisterinnen?
  val chachacha   = 118 x 20 x  9 // Waffenkarton.. "Geeignet für alles was passt!" - ach nee!

  private def zufallGen = scala.util.Random
  def zufall: KisteLeer = zufall(60,60,60)
  def zufall(maxA: Int, maxB: Int, maxC: Int): KisteLeer =  // Größer 0!
    zufallGen.nextInt(maxA-1) + 1 x
    zufallGen.nextInt(maxB-1) + 1 x
    zufallGen.nextInt(maxC-1) + 1
  def zufallKisten(maxA: Int, maxB: Int, maxC: Int): Stream[KisteLeer] =
    Stream.cons(zufall(maxA,maxB,maxC), zufallKisten(maxA, maxB, maxC))
}
