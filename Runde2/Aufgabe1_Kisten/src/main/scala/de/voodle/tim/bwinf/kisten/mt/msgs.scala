package de.voodle.tim.bwinf.kisten
package mt

import core.KistenSatz

sealed trait PackingMessage

case object HelperFinished extends PackingMessage
case class Stop(toBeComputed: Int) extends PackingMessage
case class Compute(ks: KistenSatz) extends PackingMessage
case class Ready(ks: KistenSatz) extends PackingMessage
