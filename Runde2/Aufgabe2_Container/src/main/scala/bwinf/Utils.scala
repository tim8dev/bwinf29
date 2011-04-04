package de.voodle.tim.bwinf.container

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Utils {
  type Cycle = List[Int]
  type Cycles = List[Cycle]

  import scala.util.Random
  import scala.collection.mutable.IndexedSeq
  def randPerm(n: Int) = {
    // Init array // O(n)
    // Make sure we don't convert it to an WrappedArray to often.
    val a: IndexedSeq[Int] = new Array[Int](n)
    var idx = 0
    while(idx < n) {
      a(idx) = idx + 1
      idx += 1
    }
    // randomize array // O(n)
    var i = n
    while(i >= 2) {
      val di = Random.nextInt(i)
      val swap = a(di)
      a(di) = a(i-1)
      a(i-1) = swap
      i -= 1
    }
    a
  }

  def demonstrate(n: Int) = {
    val startTime = System.currentTimeMillis
    val perm = randPerm(n)
    val cycles = Utils cyclesOf perm
    println("Time used for computing Cycles: " + (System.currentTimeMillis - startTime))
    println("Number of cycles: " + cycles.length)
    val instrs = FastAlgorithm computeFromCycles cycles
    val endTime = System.currentTimeMillis
    println("Time used: " + (endTime - startTime))
    val gleis = new Gleis(perm)
    val maschine = new Maschine(gleis)
    maschine interpret instrs
    println("Time used interpreting: " + (System.currentTimeMillis - endTime))
    gleis.waggons.zipWithIndex forall (xy => xy._1 == xy._2 + 1)
  }

  /** Return the list of disjunct cycles sorted ascending by cycle.head */
  def cyclesOf(perm: Seq[Int]): List[List[Int]] = cycles(new ListBuffer[List[Int]], perm, new Array[Boolean](perm.length))
  
  @tailrec
  private def cycles(ready: ListBuffer[List[Int]], perm: Seq[Int], handled: Array[Boolean], start: Int = 1): List[List[Int]] = // c * 
    if(start > perm.length || start < 1) ready.toList
    else {
      val aCycle = cycle(perm, start) // O(n_c)
      for (i <- aCycle) { handled(i-1) = true } // O(n_c); Side effects are not harmful, it's tailrec!
      (start to perm.length) find (i => !(handled(i-1))) match { // O(i_c) //\sum{i_c} = n
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
