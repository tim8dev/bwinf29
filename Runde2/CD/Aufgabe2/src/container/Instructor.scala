package de.voodle.tim.bwinf.container
import scala.annotation.tailrec
import scala.collection.mutable.tim.ListBuffer
import Cycler._ // import types.

object Instructor {
  def compute(perm: Seq[Int], cycler: Cycler = FastCycler): Seq[Instruction] =
    computeFromCycles(cycler cyclesOf perm)
  def computeFromCycles(cycles: Cycles): Seq[Instruction] =
    TakeCon :: computeCycle(cycles.head, cycles.tail, 1)._1.init.toList

  def alternateComputer(cycles: Cycles): Seq[Instruction] = {
    type CycleInfo = (Int, Int, ListBuffer[(Int, Int)])
    val bufferedCycles = ListBuffer() ++= cycles.map(ListBuffer() ++= _)
    val cycleInfos: ListBuffer[CycleInfo] =
      for { cycle <- bufferedCycles } yield {
        (cycle.head, cycle.max, (cycle.head +=: cycle) zip (cycle += cycle.head))
      }
    // Split into equiv classes:
    val fold = ((ListBuffer[ListBuffer[CycleInfo]](), ListBuffer[CycleInfo](), 0) /: cycleInfos) {
      case ((classes, prev, prevMax), cur @ (start, max, _)) =>
        if(start > prevMax) // new equiv class starts here
          (classes += prev, ListBuffer(cur), max)
        else // still the same equiv class
          (classes, prev += cur, math.max(prevMax, max))
    }
    val equivClasses = fold._1 += fold._2
    val classMoves = for {
      eqClass <- equivClasses
    } yield {
      ((eqClass.head._2, eqClass.head._3) /: eqClass.tail) {
        case ((prevMax, prevMoves), cur @ (start, max, moves)) =>
          (math.max(prevMax, max), prevMoves.take(start) += (-1 -> 0) ++=
            moves += (0 -> -1) ++= prevMoves.drop(start))
      }
    }
    val classInfos = for { (max, moves) <- classMoves } yield {
      (max, for { move <- moves
           instrs <- (move match {
            case (-1, 0) =>
              ListBuffer[Instruction](TakeWag, TakeCon, Rotate, PutCon, Rotate)
            case (0, -1) =>
              ListBuffer[Instruction]()
            case (x, y) =>
              ListBuffer[Instruction](Move(x -> y), Rotate, PutWag, TakeCon)
          })
      } yield instrs)
    }
    
    ((0, ListBuffer[Instruction]()) /: classInfos) {
      case ((prevMax, prevInstrs), (max, classInstrs)) =>
        null
    }._2
  }

  /**
   * Should be called, after a TakeCon!
   * When a cycle starts, all the containers in the cycles are supposed to be on the
   * container side.
   * Container are always transported on the Container side!
   */
  private def computeCycle(
    cycle: Cycle, other: Cycles, prevMax: Int): (ListBuffer[Instruction], Cycles) = {
    val max = math.max(cycle.max, prevMax) // Where is the bound of this equivalence class?

    type Step = (ListBuffer[Instruction], Cycles, Int)
    @tailrec def step(instrs: ListBuffer[Instruction], cyclesLeft: Cycles,
                      prev: Int, cur: Int): Step =
      cyclesLeft.headOption match {
        case Some(nextCycle @ (next :: _)) if prev == max && max+1 == next => // (1)
          val (cycleInstrs, _) = computeCycle(nextCycle, cyclesLeft.tail, max)
          val extraInstrs = instrs ++=
            ListBuffer(MoveRight, Rotate, TakeCon, Rotate, PutCon, Rotate) ++=
            cycleInstrs += MoveLeft
          step(extraInstrs, Nil, prev, cur)
        case Some(nextCycle @ (next :: _)) if next < cur => // (2)
          val (cycleInstrs, newCyclesLeft) =
            computeCycle(nextCycle, cyclesLeft.tail, max)
          // Move from prev to nextCycle.head (next)
          val newInstrs = instrs ++=
            ListBuffer(Move(prev -> next), Rotate, TakeCon, Rotate, PutCon, Rotate) ++=
            cycleInstrs
          step(newInstrs, newCyclesLeft, next, cur)
        case _ => // (3)
          val newInstrs = instrs ++=
            ListBuffer(Move(prev -> cur), Rotate, PutWag, TakeCon)
          (newInstrs, cyclesLeft, cur)
      }

    val erster = cycle.head
    val initial = (ListBuffer[Instruction](), other, erster)
    val (instrs, cyclesLeft, last) = (initial /: (cycle.tail :+ erster)) {
        case ((instrs, cyclesLeft, prev), cur) =>
            step(instrs, cyclesLeft, prev, cur)
      }
    (instrs, cyclesLeft)
  }
}