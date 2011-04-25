package de.voodle.tim.bwinf.kisten

case class AufteilenderPacker (t: Int = 15) extends Kistenpacker {
  def min(kisten: Seq[KisteLeer]): Kistensatz = {
    val kistengruppen = teileKisten(kisten)
    val kistensätze = for {
      kistengruppe <- kistengruppen
    } yield OptimalPacker min kistengruppe
    
    kistensätze reduceLeft { _ neben _ }
  }

  private def teileKisten(kisten: Seq[KisteLeer]) = {
    val m = (kisten.length.toDouble / t).ceil.toInt
    val startListen = Vector.fill(m)(List.empty[KisteLeer])
    ((startListen, 0) /: kisten) {
      case ((listen, idx), kiste) =>
        val liste = listen(idx)
        (listen updated (idx, kiste :: liste), (idx+1)%m)
    } match {
      case (listen, _) => listen
    }
  }
}
