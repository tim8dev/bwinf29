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

  def demonstrate(n: Int) = {
    val startTime = System.currentTimeMillis
    val perm = randPerm(n)
    val cycles = FastCycler cyclesOf perm
    println("Time used for computing Cycles: " + (System.currentTimeMillis - startTime))
    println("Number of cycles: " + cycles.length)
    val instrs = Instructor computeFromCycles cycles
    val endTime = System.currentTimeMillis
    println("Time used: " + (endTime - startTime))
    val gleis = new Gleis(perm)
    val maschine = new Maschine(gleis)
    maschine interpret instrs
    println("Time used interpreting: " + (System.currentTimeMillis - endTime))
    println("Verifying results...")
    gleis.waggons.zipWithIndex forall (xy => xy._1 == xy._2 + 1)
  }

  object IO { // IO Helpers.
    import java.io.{File,FileWriter}
    def savePerm(perm: Seq[Int])(at: String) {
      val fileWriter = new FileWriter(new File(at))
      fileWriter.write(perm mkString " ")
      fileWriter.flush
    }
    def saveInstrs(instrs: Seq[Instruction])(at: String) {
      val fileWriter = new FileWriter(new File(at))
      fileWriter.write(instrs map (_.short) mkString " ")
      fileWriter.flush
    }
  }
}