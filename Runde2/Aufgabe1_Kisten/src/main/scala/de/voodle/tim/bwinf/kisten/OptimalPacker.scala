package de.voodle.tim.bwinf.kisten

case object OptimalPacker extends Kistenpacker {
  def min(kistenListe: Seq[KisteLeer]) =
    packe(kistenListe) min Kistensatz.Ordnung.nachVolumen
 
  def packe(kistenListe: Seq[KisteLeer]) =
    (Set[Kistensatz]() /: sortiere(kistenListe)) ( packSchritt )

  protected def packSchritt(sätze: Set[Kistensatz], kiste: KisteLeer) =
    if(sätze.isEmpty)
      Set(Kistensatz(kiste :: Nil)) // KistenSatz nur mit der Kiste
    else for { satz <- sätze
               neuerSatz <- satz ++< kiste } yield neuerSatz
}