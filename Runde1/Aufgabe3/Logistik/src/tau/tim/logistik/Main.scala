package tau.tim.logistik

import java.io.File
import scala.io.Source

object Main {
  /**
   * @param args Die Kommandozeilenargumente
   */
  def main(args: Array[String]): Unit = {
    val fileName = args.headOption match { case Some(x) => x
                                           case None => "./table.txt"}
    val table =
      if(new File(fileName).exists)
        Source.fromFile(fileName)
      else
        Source.fromFile("Aufgabe3/dist/table.txt")
    TourenPlan.berechne(TourenPlan.ausDatei(table), println(_ : String))
  }

}
