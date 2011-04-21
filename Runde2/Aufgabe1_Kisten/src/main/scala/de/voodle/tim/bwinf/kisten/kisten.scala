package de.voodle.tim.bwinf.kisten

import math._

sealed abstract class Kiste extends Ordered[Kiste] {
  val a,b,c: Int
  final val v = a*b*c // cache it!

  final def +<(der: Kiste) =
    if(this >~ der)   // Passt der hinein?-
      this +<< der  // Dann packe ihn ein!
    else Set.empty // Sonst lass' es sein.

  protected def +<<(der:Kiste): Set[Kiste]

  // Folgende Methoden prüfen unabhänig vom Inhalt!
  final def >~(der: Kiste) = a > der.a && b > der.b && c > der.c
  final def >=~(der: Kiste) = a >= der.a && b >= der.b && c >= der.c

  // Benutzung intern.
  protected[kisten] def <(x: Int, y: Int, z: Int) = a < x && b < y && c < z
  protected[kisten] def tuple_/:[T](start: T)(f: (T, Int,Int,Int) => T) =
    f(f(f(f(f(f(start, a,b,c), a,c,b), b,a,c), b,c,a), c,a,b), c,b,a) // 3*2*1 = 6 Tuples.

  final def compare(der: Kiste) =
    if(a != der.a) a - der.a
    else if(b != der.b) b - der.b
    else if(c != der.c) c - der.c
    else vergleichInhalt(der)
  protected def vergleichInhalt(der: Kiste): Int

  final override def toString = baumString(0)
  private def baumString(lvl: Int): String = {
    val pref = " " * lvl + (if(lvl > 0) "\\-" else "")
    val selbst = getClass.getSimpleName + (a,b,c)
    pref + selbst + (if(kinder.isEmpty) "" else "\n") +
      kinder.map(_.baumString(lvl + 1)).
             mkString("\n")
  }

  override def equals(that: Any) = that match {
    case der: Kiste =>
      getClass() == der.getClass() && a == der.a && b == der.b && c == der.c
    case _ => false
  }
  override def hashCode = 31*(31*(31*(31*(31 + v) + a) + b) + c)

  def finde(f: Kiste => Boolean): List[Kiste] =
    if(f(this)) this :: Nil
    else {
      val kinderPfade = kinder.map(_.finde(f))
      kinderPfade.find(!_.isEmpty) map {
        path => this :: path
      } getOrElse Nil
    }

  def kinder: Seq[Kiste]
  def istLeer = kinder.isEmpty
  def alsLeer = KisteLeer(a,b,c)
  def toStream: Stream[Kiste] = Stream(this).append(kinder flatMap (_.toStream))

  def *(anzahl: Int) = List.fill(anzahl)(this)
}

// Mem. cons.: 2*8(java,scala) + 4*4(a,b,c,v) + 4(hash) = 36 -> 40 (bytes)
case class KisteLeer private[kisten](a: Int, b: Int, c: Int) extends Kiste {
  def kinder = Seq.empty

  def +(der: Kiste) = KisteHalb(a,b,c, der)
  protected def +<<(der: Kiste) = Set(KisteHalb(a,b,c, der))

  protected def vergleichInhalt(der: Kiste) = der match {
    case _: KisteLeer => 0 // Zwei leere Kisten gleicher Größe sind die gleichen
    case _ => -1                // Sonst muss dieser Leere als Kleinerer weichen
  }

  override val hashCode = // kann nie <0 sein; Nutze den Companion, für die Hashfunktion.
    31*(31*(31* + super.hashCode) + KisteLeer.hashCode)

  // Überschreibe wegen Types!
  override def alsLeer = this
  override def *(anzahl: Int) = List.fill(anzahl)(this)
}

// Mem. consump.: 2*8(java,scala) + 8(links ref) + 4*4(a,b,c,v) + 4(hash) = 44 -> 48 (bytes)
case class KisteHalb private[kisten](a: Int, b: Int, c: Int, links: Kiste) extends Kiste {
  override def istLeer = false
  def kinder = Seq(links)
  def freiFür(der: Kiste) = (false tuple_/: links) {
      (prev, x,y,z) => prev ||
        der < (a-x,b,c) || // entspricht: Kiste(a-x,b,c) ⊃ der ; < wg. -1
        der < (a,b-y,c) ||
        der < (a,b,c-z)
      }

  def ersetzeLinks(nl: Kiste) = KisteHalb(a,b,c, nl)

  def +(der: Kiste): KisteVoll = Kiste(a,b,c, links, der)

  protected def +<<(der: Kiste) = {
      val neueLinks = links +< der
      val kisten: Set[Kiste] = neueLinks.map(ersetzeLinks(_))
      if(this freiFür der)
        kisten + (this + der)
      else
        kisten
  }
  protected def vergleichInhalt(der: Kiste) = der match {
      case leer: KisteLeer => 1       // Ein leerer! Ach wie Tolle!
      case halb: KisteHalb =>
        this.links compare halb.links // !Hier! spielt der Inhalt eine Rolle!
      case _ => -1                    // Schau! Da bleibt nur noch der Volle..
    }

  override val hashCode = // wand sollte nie <0 sein
    31*(31*(31*(31* + super.hashCode) + links.hashCode) + KisteHalb.hashCode)
}

// Mem. consump.: 2*8(java,scala) + 2*8(links,rechts ref) + 4*4(a,b,c,v) + 1(lgr) + 4(hash) = 53 -> 56 (bytes) [64]
case class KisteVoll private[kisten](a: Int, b: Int, c: Int, links: Kiste, rechts: Kiste) extends Kiste {
  override def istLeer = false
  //require(links ⊆ rechts, "Links muss kleiner-gleich rechts sein!")
  def kinder = Seq(links, rechts)

  // Nicht klar, ob nl >= rechts, oder nicht. (bzw. links >= nr)
  def ersetzeLinks(nl: Kiste)  = Kiste(a,b,c, nl, rechts)
  def ersetzeRechts(nr: Kiste) = Kiste(a,b,c, links,  nr)

  private val linksGleichRechts = links == rechts
  protected def +<<(der: Kiste) = {
    val linkeSeite: Set[Kiste] = (links +< der).map(ersetzeLinks(_))
    if(linksGleichRechts)
      linkeSeite
    else {
      val rechteSeite = (rechts +< der).map(ersetzeRechts(_))
      linkeSeite ++ rechteSeite
    }
  }
  protected def vergleichInhalt(der: Kiste) = der match {
    case voll: KisteVoll =>                             // Der Inhalt entscheide!
      val linksDiff = this.links compare voll.links // Prüf erst die linke Seite!
      if(linksDiff != 0) linksDiff             // Sind es auch nicht die gleichen?
      else this.rechts compare voll.rechts     // Dann müssen die Rechten reichen
    case _ => 1                           // Sonst muss der kleine and're weichen!
  }

  override val hashCode =
    31*(31*(31*(31*(31 + super.hashCode) + links.hashCode) + rechts.hashCode)
        + KisteVoll.hashCode)
}

object Kiste {
  def ordne(x: Int, y: Int, z: Int) = {
    var (a,b,c) = (x,y,z)
    var tmp = 0
    if(a < b) { tmp = a; a = b; b = tmp }
    if(a < c) { tmp = a; a = c; c = tmp }
    // es gilt jetzt: a >= b && a >= c
    if(b < c) { tmp = b; b = c; c = tmp }
    // es gilt jetzt: a >= b >= c
    // also wenn eine Zahl nicht positiv ist, dann auf jeden Fall auch c
    if(c < 0) throw new IllegalArgumentException("Negative Werte sind nicht erlaubt!") // Negativ!.. unmöglich!
    else      (a,b,c)
  }
  // Objekterzeuger [Hilfs-]Methoden:
  def apply(x: Int, y: Int, z: Int) = {
    val (a,b,c) = ordne(x,y,z)
    new KisteLeer(a,b,c)
  }
  def apply(x: Int, y: Int, z: Int, links: Kiste) = {
    val (a,b,c) = ordne(x,y,z)
    new KisteHalb(a,b,c, links)
  }
  
  def apply(x: Int, y: Int, z: Int, links: Kiste, rechts: Kiste) = {
    val (a,b,c) = ordne(x,y,z)
    // links muss >= rechts sein!
    if(links >= rechts) new KisteVoll(a,b,c, links, rechts)
    else                new KisteVoll(a,b,c, rechts, links)
  }

  object Ordnung {
    object nachVolumen extends Ordering[Kiste] {
      def compare(dieser: Kiste, anderer: Kiste) = dieser.v - anderer.v
    }
    object eindeutig extends Ordering[Kiste] {
      def compare(dieser: Kiste, anderer: Kiste) = dieser compare anderer
    }
  }

  implicit def intToPartialKiste(a: Int): PartialKisteA = PartialKisteA(a)
}

// Helper für schöne Syntax:
case class PartialKisteA(a: Int) {
  def x(b: Int) = PartialKisteAB(a,b)
}
case class PartialKisteAB(a: Int, b: Int) {
  def x(c: Int) = Kiste(a,b,c)
}