package de.voodle.tim.bwinf.kisten

object LogStufe extends Enumeration {
  type LogStufe = Value
  val Extra    = Value(20)
  val Ergebnis = Value(10)
}
object KistenLogger {
  import java.io.{File, FileWriter}
  val suffix = "_kisten.log"
  def getNextFileWriter =
    Stream from 0 map (i => new File("./" + i + suffix)) find {
      ! _.exists
    } map {
      file => new FileWriter(file)
    } getOrElse (throw new AssertionError("No file found!"))
  implicit def ausgabeAusFileWriter(fw: FileWriter): String => Any = {
    str => fw.write(str); fw.flush
  }
}
case class KistenLogger(printer: String => Any, dateiAusgabe: String => Any) {
  import LogStufe._
  def logge(zeile: =>String)(implicit print: Boolean = true) =
    if(print) {
      val ausgabenZeile = zeile + "\n" // By-name!
      printer(ausgabenZeile)
      dateiAusgabe(ausgabenZeile)
    } else ()
}
