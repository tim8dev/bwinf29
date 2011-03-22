package tau.tim.logistik

import scala.io.Source

object Main {
  /**
   * @param args Die Kommandozeilenargumente
   */
  def main(args: Array[String]): Unit = {
    val fileName = args.headOption match { case Some(x) => x
                                           case None => "./table.txt"}
    val table = Source.fromFile(fileName)
    TourenPlan.berechne(TourenPlan.ausDatei(table), println(_ : String))
  }

}
