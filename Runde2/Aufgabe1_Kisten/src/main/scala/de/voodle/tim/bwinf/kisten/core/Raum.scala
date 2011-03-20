package de.voodle.tim.bwinf
package kisten.core

// Memory consumption: 8 + 4*3(a,b,c) + 4(hash) + 4(v) = 28 -> 32
case class Raum(a: Int, b: Int, c: Int) extends Ordered[Raum] {
  val v = a * b * c // Volumen

  def ⊃(der: Raum) = this >~ der
  def ⊂(der: Raum) = this <~ der
  def ⊆(der: Raum) = this <=~ der
  def ⊇(der: Raum) = this >=~ der
  def ≠(der: Raum) = this ~ der

  // Ein Raum ist entweder ⊂, ⊃, ≠, oder == einem anderem (Halbordnung!)
  def <=~(der: Raum) = der >=~ this
  def >=~(der: Raum) = // Die anderen checks werden nicht gebraucht (Beweis!)
    this >= (der.a, der.b, der.c) /*|| this >= (d,f,e) ||
    this >= (e,d,f) || this >= (e,f,d) ||
    this >= (f,d,e) || this >= (f,e,d)*/
  def  <~(der: Raum) = this != der && this <=~ der
  def  >~(der: Raum) = this != der && this >=~ der
  def  =~(der: Raum) = this == der
  def   ~(der: Raum) = !(this <~ der || this >=~ der) // keine Relation

  def - (wand: Int) = Raum(a-wand, b-wand, c-wand)

  private def >=(d: Int, e: Int, f: Int) = a >= d && b >= e && c >= f
  private def > (d: Int, e: Int, f: Int) = a >  d && b >  e && c > f

  private def --(d: Int, e: Int, f: Int): Set[Raum] =
    if(this >= (d,e,f))
      Set( Raum.make(a-d, b, c).get,
           (if(b-e >= c) Raum(a, b-e, c) else Raum(a, c, b-e)),
           //Raum.make(a, b-y, c).get + // Get ist sicher, weil (a,b,c) >= (x,y,z)
           Raum(a, b, c-f)) // c-z ist garantiert < a und < b!
    else Set()

  def -- (der: Raum): Set[Raum] =
    if (this >~ der)
      der.foldLeftPossibleTuples(Set[Raum]()) {
         (fertig: Set[Raum], d,e,f) =>
            val moeglichkeiten = this -- (d,e,f)
            (fertig /: moeglichkeiten) { (vor, r) =>
              if(!vor.contains(r) && r >= (0,0,0) && !vor.exists(_ >=~ r))
                vor + r
              else
                vor
            }
      }
    else Set.empty

  private def foldLeftPossibleTuples[T](start: T)(f: (T, Int,Int,Int) => T) =
    f(f(f(f(f(f(start, a,b,c), a,c,b), b,a,c), b,c,a), c,a,b), c,b,a)

  override val hashCode = // a sollte nie negativ sein.
    31* (31* (31* (31 + a ) + b) + c) + v
  
  override def equals(other: Any) = other match {
    case Raum(d,e,f) => a == d && b == e && c == f
    case _ => false
  }
  def compare(der: Raum) = if(a != der.a) a - der.a
                      else if(b != der.b) b - der.b
                      else if(c != der.c) c - der.c
                      else 0 // Sonst gleich.

  override def toString = "(" + a + " x " + b + " x " + c + ")"
  def k: KisteLeer = KisteLeer(a,b,c)
}

object Raum {
  def make(x: Int, y: Int, z: Int) = {
    var tmp = 0
    var (a,b,c) = (x,y,z)
    if(a < b) { tmp = a; a = b; b = tmp }
    if(a < c) { tmp = a; a = c; c = tmp }
    // es gilt jetzt: a >= b && a >= c
    if(b < c) { tmp = b; b = c; c = tmp }
    // es gilt jetzt: a >= b >= c
    // also wenn eine Zahl nicht positiv ist, dann auf jeden Fall auch c
    if(c < 0) None // Raum negativ!.. unmöglich!
    else      Some(Raum(a,b,c))
  }

  object Ordnung {
    object nachVolumen extends Ordering[Raum] {
      def compare(dieser: Raum, anderer: Raum) = dieser.v - anderer.v
    }
    object nachGroesse extends Ordering[Raum] {
      def compare(dieser: Raum, anderer: Raum) =
        dieser compare anderer
    }
  }

  implicit def intToPartialRaum(a: Int): PartialRaumA = PartialRaumA(a)
  implicit def raumToKarton(raum: Raum): Kiste = Kiste(raum.a, raum.b, raum.c)
}

// Helper für schöne Syntax:
case class PartialRaumA(a: Int)          { def x(b: Int) = PartialRaumAB(a,b) }
case class PartialRaumAB(a: Int, b: Int) { def x(c: Int) = Raum.make(a,b,c).get }
