package tau.tim.drehzahl

import scala.collection.mutable.ListBuffer

object Spiel {
  private def spiel(greet: Karten=>String, play: Karten=>(Karten,Boolean),
                    anfangsKarten: Karten, end: Karten=>String) : Int = {
    var karten = anfangsKarten
    println()
    println(greet(karten))
    var finished = false
    while(!finished) {
      val res = play(karten)
      karten = res._1
      finished = res._2
    }
    println(end(karten))
    karten.punkte
  }

  def optimalSpiel(zahlen: Iterator[Int], anfangsKarten: Karten) : Int = {
    val function = (k : Karten) => {
      println("Karten sind: " + k)
      val zahl = if(zahlen.hasNext) zahlen.next else Wuerfel.wuerfeln()
      println("Gewürfelt wurde " + zahl)
      val streichbar = k.streichbar(zahl)
      if(streichbar.isEmpty) {
        println("Keine Streichmöglichkeit!\n")
        (k,true)
      } else {
        println (" => Streichmöglichkeiten: " + streichbar.mkString(","))
        val streichPunkte = for(s <- streichbar)
          yield k.streiche(s).gewichtetePunkte -> s
        val beste = streichPunkte.max
        val karten = beste._2
        println("Es wird " + karten +
                " gestrichen (erwartete Punktzahl: " + beste._1 + ")\n")
        (k.streiche(karten),false)
      }
    }
    spiel(_=>"OptimalSpiel startet.",
          function, anfangsKarten,
          "Spielende! - Es wurden " + _.punkte + " Punkte erreicht!")
  }

  def spielen(anfangsKarten : Karten) {
    val gewuerfelt = new ListBuffer[Int]
    var karten = anfangsKarten
    var finished = false
    while(!finished) {
      println("Karten sind: " + karten)
      val zahl = Wuerfel.wuerfeln()
      gewuerfelt += zahl
      println("Würfeln...... " + zahl)
      println("Geben Sie nun ein, welche Zahlen Sie streichen wollen.")
      println("Trennen Sie Zahlen mit Komma.")
      val streichbar = karten.streichbar(zahl)
      if(streichbar.isEmpty) {
        finished = true
        println("EndKarten: " + karten)
        println("Spielende! - Sie haben " + karten.punkte + " Punkte erreicht!")
      } else {
        karten = karten streiche
                 leseKarten(readLine())(k =>{
                     k.punkte == zahl
                 })
      }
    }
    println()
    val endFunct = (p : Int) => {
      val comp = p compare optimalSpiel(gewuerfelt.iterator, anfangsKarten)

      if(comp > 0) "GLÜCKWUNSCH! Sie waren besser als der Computer!"
      else if(comp == 0) "Gratulation. Sie waren genauso gut wie der Computer!"
      else "Tja...haha! Der Computer war wohl besser =P"
    }
    println(endFunct(karten.punkte))
  }

  def leseKarten(input: String)(implicit bed: (Karten)=>Boolean = _=>true) = {
    var karten: Karten = null
    var success = false
    do{
      val zahlen : Seq[Int] = input.trim.split(Array(',','&',' ','|')).toList.map(_.toInt)
      karten = Karten(zahlen: _*)
      success = bed(karten)
      if(!success) println("Geben Sie eine gültige Zahlenfolge zum Streichen an!")
    } while(!success)
    karten
  }

  private def streiche(karten : Karten, zahlen : Seq[Int]) : Option[Karten] =
    try
      Some(karten streiche Karten(zahlen.toList))
    catch {
      case ex: IllegalArgumentException => None
    }
}

