package de.voodle.tim.bwinf.container

object Utils {
  import scala.util.Random
  import scala.collection.mutable.IndexedSeq
  def randPerm(n: Int) = {
    // Make sure we don't convert it to an WrappedArray to often.
    val a: IndexedSeq[Int] = new Array[Int](n)
    // Init array // O(n)
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
