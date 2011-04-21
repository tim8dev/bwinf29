package de.voodle.tim.bwinf.kisten

import scala.actors._, Actor._

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

// TODO: Complete if there's time.
case class PackAktor (kistenSeq: Seq[KisteLeer],
                      sammler: SammelAktor,
                      versetzung: Int, tf: Int) extends Actor {
  def act {
    val (_, kisten) = ((-versetzung, List[KisteLeer]()) /: kistenSeq) {
      case ((abstand, liste), kiste) =>
        if(abstand == 0) (-tf, kiste :: liste)
        else (abstand +1, liste)
    }
    val min = OptimalPacker.min(kisten)
    
  }
}

sealed trait SammelAktor extends Actor
case class WurzelAktor() {
  def act =
    receive {
      case ks: Kistensatz =>
        receive {
          case 'Return => reply(ks)
        }
    }
}
case class KnotenAktor(sammler: SammelAktor) {
  private var ks: Option[Kistensatz] = None
  def act {
    react {
      case nks: Kistensatz =>
        ks match {
          case None =>
            ks = Some(nks)
            act
          case Some(oks) =>
            sammler ! (nks neben oks) // TODO!
        }
    }
  }
}
object SammelAktor {
  def aktorenIntialisieren(root: SammelAktor, m: Int, l: Int, r: Int) {
    SammelAktor
  }
}