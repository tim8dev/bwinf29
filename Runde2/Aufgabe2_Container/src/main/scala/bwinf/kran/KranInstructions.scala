package de.voodle.tim.bwinf.container
package kran

import scala.collection.mutable.ListBuffer

import mci.KranInterface

/*trait KranInterface {
  def move(left: Boolean)
  def putContainer(left: Boolean)
  def takeContainer(left: Boolean)
  def greiferBewegen(left: Boolean)
  def rotate
}*/

class KranInstrsBuffer {
  private val buffer = new ListBuffer[KranInterface => Unit]
  def add(instr: KranInterface => Unit) {
    buffer += instr
  }
  def createInstructions(kran: KranInterface) =
    new KranInstructions(buffer.toList, kran)
}

class KranInstructions(private var instrs: List[KranInterface => Unit],
                       private val kran: KranInterface) {
  def hasNext: Boolean = !instrs.isEmpty
  def next {
    instrs.head(kran)
    instrs = instrs.tail
  }
}
