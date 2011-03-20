package de.voodle.tim.bwinf.kisten.core

import org.specs._
import org.scalacheck._

object KistenSpec extends Specification with ScalaCheck with KistenGen {
  import Prop.forAll
  "Two Kartons k,l" should {
    "have an equal hashCode if they are equal" in {
      forAll{(x: Kiste, y: Kiste) =>
        if(x == y) x.hashCode == y.hashCode
        else true
      } must pass (display(minSize -> 200, maxSize -> 5000, minTestsOk -> 222)) // high test rate!, rare occurence.
    }
  }
  
}

trait KistenGen extends RaumGen {
  import Gen._
  import Arbitrary.arbitrary
  
  val leerGen: Gen[KisteLeer] = for(room <- arbitrary[Raum]) yield room k

  val halbGen: Gen[KisteHalb] =
    (for{room <- arbitrary[Raum]
        left <- genKiste} yield ((room k) + left)) suchThat (k => k.kinder.forall(k ⊃ _))

    val vollGen = (for {
      room <- arbitrary[Raum]
      left <- genKiste
      right <- genKiste
    } yield Kiste(room.a, room.b, room.c, left, right)) suchThat (k => k.kinder.forall(k ⊃ _))

  def genKiste: Gen[Kiste] = frequency(3 -> leerGen,
                                         1 -> halbGen,
                                         1 -> vollGen)

  implicit val arbitKarton: Arbitrary[Kiste] = Arbitrary(genKiste)
}