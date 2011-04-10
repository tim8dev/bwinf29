package de.voodle.tim.bwinf.kisten.core
package online

abstract trait Strategie {
  // Syntactic sugar :)
  def apply(kisten: KistenSatz)(der: KisteLeer)
           (implicit vergleich: KistenVergleich = StandardVergleich) =
    finde(kisten)(der)(vergleich)

  protected def finde(kisten: KistenSatz)(der: KisteLeer)
                     (vergleich: KistenVergleich): Option[KistenSatz]
}

object Strategie {
  private[online] def pfadErsetzer(pfad: List[Kiste]): (Kiste,Kiste) => Kiste =
    (_, _) match {
      case (kg: KisteHalb, kl) => kg ersetzeLinks kl
      case (kg: KisteVoll, kl) =>
        if(pfad.contains(kg.links)) kg ersetzeLinks  kl
        else                        kg ersetzeRechts kl
      case _ => throw new IllegalArgumentException("Leere Kisten im Pfad nicht erlaubt!")
    }
}
import Strategie._

object FindeHalbleeren extends Strategie {
  protected def finde(kisten: KistenSatz)(der: KisteLeer)(vergleich: KistenVergleich) =
    kisten.find { _ match {
        case kh: KisteHalb =>
          (kh ⊃ der) && kh.freiFür(der)
        case _ => false
      }
    } match {
      case Nil => None
      case pfad =>
        val alteKiste = pfad.head
        val neueKiste = (pfad :\ (der: Kiste)) { (_, _) match {
            case (kg: KisteHalb, kl: KisteLeer) => kg + kl
            case (k1, k2) => pfadErsetzer(pfad)(k1, k2)
          }}
        Some(kisten - alteKiste + neueKiste)
    }
}
object FindeGrößerenLeeren extends Strategie {
  protected def finde(kisten: KistenSatz)(der: KisteLeer)(vergleich: KistenVergleich) =
    kisten.find {
      big => (big ⊃ der) && big.istLeer
    } match {
      case Nil => None
      case pfad =>
        val alteKiste = pfad.head
        val neueKiste = (pfad :\ (der: Kiste)) { (_, _) match {
            case (kg: KisteLeer, kl: KisteLeer) => kg + kl
            case (k1, k2) => pfadErsetzer(pfad)(k1, k2)
          }}
        Some(kisten - alteKiste + neueKiste)
    }
}
object FindeZwischenraum extends Strategie {
  protected def finde(kisten: KistenSatz)(der: KisteLeer)(vergleich: KistenVergleich) =
    kisten.find { k => (k ⊃ der) && (k match {
        case kh: KisteHalb => // Es ist sicher noch Platz für einen Zwischenraum
           (der ⊃ kh.links)
        case kv: KisteVoll => // Nur dann ist Platz, wenn 'der' noch neben den anderen reinpasst.
          val links  = Kiste(kv.a,kv.b,kv.c, kv.links)
          val rechts = Kiste(kv.a,kv.b,kv.c, kv.rechts)
          ((der ⊃ kv.links)  && links.freiFür(der)) ||
          ((der ⊃ kv.rechts) && rechts.freiFür(der))
        case _ => false
      })
    } match {
      case Nil => None
      case pfad =>
        val alteKiste = pfad.head
        val neueKiste = (pfad :\ (der: Kiste)) { (_, _) match {
            case (kg: KisteHalb, kl: KisteLeer) => kg ersetzeLinks (kl + kg.links)
            case (kg: KisteVoll, kl: KisteLeer) =>
              if(kg.links ⊃ kl) kg ersetzeLinks  (kl + kg.links)
              else               kg ersetzeRechts (kl + kg.rechts)
            case (k1, k2) => pfadErsetzer(pfad)(k1, k2)
          }}
        Some(kisten - alteKiste + neueKiste)
    }
}
object FindeKleinereWurzel extends Strategie {
  protected def finde(kisten: KistenSatz)(der: KisteLeer)(vergleich: KistenVergleich) =
    kisten.kistenSet.find(der ⊃ _) map {
      kWurzel => kisten - kWurzel + (der + kWurzel)
    }
}