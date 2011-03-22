package tau.tim.drehzahl

object Main {

  /**
   * Haupteintrittspunkt des Programmes.
   * Es wird zunächst berechnet, welcher Punktedurchschnitt erwartet wird.
   * Danach wird im Experiment ausprobiert, wieviel Punkte erreicht werden können.
   *
   * @param args Die Kommandozeilenargumente
   */
  def main(args: Array[String]): Unit = {
    println("Geben Sie bitte an, mit welchen Karten fortgefahren werden soll.")
    println("(Drücken Sie einfach <Enter> wenn " +
            "der Standardkartensatz(" + Karten.standard + ") verwendet werden soll")
    val input = readLine()
    val karten = if(input.length < 1) Karten.standard
                 else Spiel.leseKarten(input)

    var continue = true
    do {
      println("\nKarten sind: " + karten + "\n")
      
      val modi: Map[String,(String, Karten=>Any)] = Map(
        "s" -> ("ein neues Spiel zu starten" -> spiel),
        "b" -> ("die Durchschnittspunktzahl zu berechnen" -> theoretisch),
        "e" -> ("empirisch die Durchschnittspunktzahl zu errechnen" -> empirisch)
      )

      for(mod <- modi) println("Geben Sie " + mod._1 + " ein, um " + mod._2._1)
      println("Drücken Sie einfach <Enter> um zu beenden.")

      val in = readLine()
      val mod = modi.get(in)
      continue = mod match {
        case Some(m) => m._2(karten); true
        case _ => false
      }
    } while(continue);
  }

  def spiel(karten: Karten) {
    Spiel.spielen(karten)
  }

  def theoretisch(karten: Karten) = {
    println("Berechnung für " + karten)
    val punktzahl = karten.gewichtetePunkte
    println("Erwartete durschnittliche Punktzahl: " + punktzahl)
    punktzahl
  }

  def empirisch(karten: Karten): Double = {
    println("Geben Sie bitte an, wie oft ein Spiel ausgeführt werden soll.")
    val anzahl = readLine().toInt
    empirisch(anzahl, karten)
  }

  def empirisch(testAnzahl: Int, karten: Karten) = {
    var sum: Double = 0
    for(i <- 0 until testAnzahl)
      sum += Spiel.optimalSpiel(Nil.iterator, karten)
    val avg = sum / testAnzahl
    println("\n\nIm Experiment ("+ testAnzahl + "x): " + avg)
    if(karten.punkte < 30) {
      val theo = karten.gewichtetePunkte
      println("Theoretisch berechnet: " + avg)
      val unterschied = math.abs(theo - avg)
      println("Unterschied: " + unterschied)
    }
    avg
  }

}
