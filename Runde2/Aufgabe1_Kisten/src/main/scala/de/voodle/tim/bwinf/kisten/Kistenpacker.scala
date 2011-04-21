package de.voodle.tim.bwinf.kisten

trait Kistenpacker {
  def min(kisten: Seq[KisteLeer]): Kistensatz
  // Sortiert wird von Groß nach Klein!
  // D'rum muss die Ordnung 'falsch'rum sein
  def sortiere(input: Seq[KisteLeer]): List[KisteLeer] =
    input.toList.sorted(Kiste.Ordnung.nachVolumen.reverse)
}