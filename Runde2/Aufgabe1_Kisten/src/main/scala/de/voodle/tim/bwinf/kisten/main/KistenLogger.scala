package de.voodle.tim.bwinf.kisten.main
import scala.annotation.tailrec

object LogStufe extends Enumeration {
  type LogStufe = Value
  val Extra = Value(20)
  val Ergebnis = Value(10)
}

object KistenLogger {
  import java.io.{File, FileWriter}
  val fileSuffix = "_kisten.log"
  def getNextFileWriter = {
    @tailrec def findFile(i: Int): File = {
      val file = new File("./" + i + fileSuffix)
      if(file.exists)
        findFile(i + 1)
      else {
        file.createNewFile
        file
      }
    }
    new FileWriter(findFile(0))
  }
  implicit def ausgabeAusFileWriter(fw: FileWriter): String => Any = {
    str => fw.write(str); fw.flush
  }
}

import LogStufe._
case class KistenLogger(ausgabe: String => Any, ergebnisAusgabe: String => Any) {
  def logge(zeile: =>String)(implicit stufe: LogStufe = Extra) { // By-name!
    val ausgabenZeile = zeile
    stufe match {
      case Extra =>
        ausgabe(ausgabenZeile)
      case Ergebnis =>
        ausgabe("[Datei:] \n" + ausgabenZeile)
        ergebnisAusgabe(ausgabenZeile)
    }
  }
}
