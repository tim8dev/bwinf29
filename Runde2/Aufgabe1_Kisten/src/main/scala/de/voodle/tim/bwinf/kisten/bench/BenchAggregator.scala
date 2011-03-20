package de.voodle.tim.bwinf.kisten
package bench

import core._
import main.StandardKisten._

case class BenchAggregator(kisten: Int=>Seq[KisteLeer]) {
  def apply(packer: Seq[KisteLeer]=>KistenPacker)(times: Int): Map[Int,Int] =
    (Map[Int,Int]() /: (0 until times)) {
      (prev,idx) =>
        val min = packer(kisten(idx)).min
        prev + (idx -> min.v)
    }
}

object BenchAggregator {
  case class KistenErzeuger(maxGröße: (Int, Int, Int), anzahlKisten: Int) {
    private var kisten: List[Seq[KisteLeer]] = Nil
    def kistenListe = kisten
    def apply(idx: Int): Seq[KisteLeer] = kisten.length match {
      case len if len > idx => kisten(len-1-idx)
      case len => // Kisten von idx..len erzeugen
        kisten :::= List.fill(idx-len+1)(zufallKisten(maxGröße._1, maxGröße._2, maxGröße._3) take anzahlKisten toList)
        kisten.head
      }
  }

  def apply(ke: KistenErzeuger): BenchAggregator = BenchAggregator(ke(_))
  def apply(maxGröße: (Int, Int, Int), anzahlKisten: Int): BenchAggregator =
    BenchAggregator(KistenErzeuger(maxGröße, anzahlKisten))

  def fürOptimumPacker = BenchAggregator((80,80,80), 12)
}
