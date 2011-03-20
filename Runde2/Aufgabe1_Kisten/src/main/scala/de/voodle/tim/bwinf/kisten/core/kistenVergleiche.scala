package de.voodle.tim.bwinf.kisten.core

abstract trait KistenVergleich {
  def fitIn(karton: Kiste, in: Kiste): Boolean
}

object StandardVergleich extends KistenVergleich {
  def fitIn(karton: Kiste, in: Kiste) = in ⊃ karton
}

