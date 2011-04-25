package de.voodle.tim.bwinf.container

object Utils {
  import scala.util.Random
  import scala.collection.mutable.IndexedSeq
  def randPerm(n: Int) = {
    // Make sure we don't convert it to an WrappedArray to often.
    val a: IndexedSeq[Int] = new Array[Int](n)
    // Init array // O(n)
    for (idx <- 0 until n) a(idx) = idx + 1
    // randomize array // O(n)
    for (i <- n to 2 by -1) {
      val di = Random.nextInt(i)
      val swap = a(di)
      a(di) = a(i-1)
      a(i-1) = swap
    }
    a // return array
  }

  def demonstrate(perm: Seq[Int], print: Boolean = true) = {
    val startTime = System.currentTimeMillis
    val cycles = FastCycler cyclesOf perm
    println("Time used for computing Cycles: " +
            (System.currentTimeMillis - startTime))
    if(print) {
      println("The computed Cycles are: ")
      println(cycles map (_.mkString("(", " ", ")")) mkString ("  ", "\n  ", ""))
    }
    println("Number of cycles: " + cycles.length)
    val instrs = Instructor computeFromCycles cycles
    val endTime = System.currentTimeMillis
    if(print) {
      println("The generated Instructions (shortened) are: ")
      val instrsGroups = instrs.view.take(20*12).map(_.short).grouped(12)
      println(instrsGroups.map(_.mkString(" : "))
              .mkString(" [ ", "\n : ",
                        if(instrs.lengthCompare(20*12)>0) " ..." else " ] "))
    }
    println("Time used computing Instructions: " + (endTime - startTime))
    val gleis = new Gleis(perm)
    val maschine = new Maschine(gleis, print)
    maschine interpret instrs
    println("Time used interpreting: " + (System.currentTimeMillis - endTime))
    if(print) {
      println("Gleis: ")
      println(gleis)
    }
    println("Verifying results...")
    gleis.waggons.zipWithIndex forall (xy => xy._1 == xy._2 + 1)
  }

  def demonstrate(n: Int, print: Boolean): Boolean = {
    val perm = randPerm(n)
    if(print) {
      println("Permutation (shortened) is:")
      println(perm.take(480).mkString(
          "  (", " ", if(perm.lengthCompare(480) > 1) " ..." else ")"))
    }
    demonstrate(perm, print)
  }
}