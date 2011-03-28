package de.voodle.tim.bwinf.container

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Utils {
  type Cycle = List[Int]
  type Cycles = List[Cycle]

  import scala.util.Random
  def randPerm(n: Int) = {
    val a = (0 until n).toArray // O(n)
    for(i <- n to 2 by -1) { // O(n)
      val di = Random.nextInt(i)
      val swap = a(di)
      a(di) = a(i-1)
      a(i-1) = swap
    }
    a map (_ + 1) // from 0..n -> 1...n
  }

  /** Return the list of disjunct cycles sorted ascending by cycle.head */
  def cyclesOf(perm: Seq[Int]): List[List[Int]] = cycles(new ListBuffer[List[Int]], perm, new Array[Boolean](perm.length))
  
  @tailrec
  private def cycles(ready: ListBuffer[List[Int]], perm: Seq[Int], handled: Array[Boolean], start: Int = 1): List[List[Int]] = // c * 
    if(start > perm.length || start < 1) ready.toList
    else {
      val aCycle = cycle(perm, start) // O(n_c)
      for (i <- aCycle) { handled(i-1) = true } // O(n_c); Side effects are not harmful, it's tailrec!
      (start to perm.length) find (i => !(handled(i-1))) match {
        case Some(next) =>
          cycles(ready += aCycle, perm, handled, next)
	case None =>
	  (ready += aCycle).toList // O(1)
      }
    }
  /** Small helper function, finding one cycle. */
  private def cycle(perm: Seq[Int], start: Int): List[Int] = {// O(n_c)
    @tailrec def step(ready: ListBuffer[Int], idx: Int): Cycle = // O(n_c)
      if(start == idx) ready.toList // O(1)
      else        step(ready += idx,perm(idx - 1))
    (start :: step(new ListBuffer[Int], perm(start - 1)))
  }

  object IO { // IO Helpers.
    import java.io.{File,FileWriter}
    def savePerm(perm: Seq[Int])(at: String) {
      val file = new File(at)
      val str = perm mkString " "
      val fileWriter = new FileWriter(file)
      fileWriter.write(str)
      fileWriter.flush
    }
    def saveInstrs(instrs: Seq[Instruction])(at: String) {
      val file = new File(at)
      val str = instrs map (_.short) mkString " "
      val fileWriter = new FileWriter(file)
      fileWriter.write(str)
      fileWriter.flush
    }
  }
}
