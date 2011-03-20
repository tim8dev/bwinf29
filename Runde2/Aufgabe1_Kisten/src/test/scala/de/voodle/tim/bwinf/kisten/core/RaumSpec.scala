package de.voodle.tim.bwinf.kisten.core

import org.specs._
import org.scalacheck._

object RaumSpec extends Specification with ScalaCheck with RaumGen {
  import Prop.forAll
  "Two Rooms x,y" should {
    "yield true for y <=~ x if and only if x >=~ y" in {
      forAll { (x: Raum, y: Raum) =>
             if(x >=~ y) y <=~ x
        else if(y <=~ x) false
        else true } must pass
    }
    "match one and only one of the following relations: >~, <~, =~, ~" in {
      forAll{(x: Raum, y: Raum) => (x >~ y) ^ (x <~ y) ^ (x =~ y) ^ (x ~ y)
           /*if(x >~ y) !(x <~ y || x =~ y || x ~ y)
        else if(x <~ y) !(x >~ y || x =~ y || x ~ y)
        else if(x =~ y) !(x <~ y || x >~ y || x ~ y)
        else if(x ~ y)  !(x <~ y || x >~ y || x =~ y)
        else false */
      } must pass
    }
    "have an equal hashCode if they are equal" in {
      forAll{(x: Raum, y: Raum) =>
        if(x == y) x.hashCode == y.hashCode
        else true
      } must pass (display(minSize -> 200, maxSize -> 5000, minTestsOk -> 2222)) // high test rate!, rare occurence.
    }
    "yield 0 for x compare y if they are equal" in {
      forAll{(x: Raum, y: Raum) =>
        if(x == y) (x compare y) == 0
        else true
      } must pass (display(minSize -> 200, maxSize -> 5000, minTestsOk -> 4000)) // high test rate!, rare occurence.
    }
    "yield only rooms r with x >=~ r for x -- y" in {
      forAll{(x: Raum, y: Raum) =>
          val rooms = x -- y
          rooms.forall(x >=~ _)
      } must pass (display(minSize -> 20, maxSize -> 500, minTestsOk -> 100))
    }
    "yield a non-empty Set for x -- y if and only if x >~ y" in {
      forAll{(x: Raum, y: Raum) =>
        val rooms = x -- y
        if(x >~ y) !rooms.isEmpty
        else rooms.isEmpty
      } must pass (display(minSize -> 20, maxSize -> 500, minTestsOk -> 100))
    }
  }
  import Raum._
  "The (1 x 1 x 1) Room" should {
    "fit into any other Room" in {
      forAll{(x: Raum) =>
        if(x != (1 x 1 x 1)) x >=~ (1 x 1 x 1)
        else true
      } must pass
    }
    "have volume 1" in {
      (1 x 1 x 1).v must be equalTo(1)
    }
    "not be bigger than any other Room" in {
      forAll{(x: Raum) =>
        !((1 x 1 x 1) >~ x)
      } must pass
    }
    "be smaller than or equal to any other (generated) Room" in {
      forAll{(x: Raum) =>
       (1 x 1 x 1) < x
      } must pass
    }
  }
}

trait RaumGen {
  import Raum._

  implicit val raumGen: Gen[Raum] = for {
    a <- Gen.choose(2, 60)
    b <- Gen.choose(1, 40)
    c <- Gen.choose(1, 40)
  } yield a x b x c

  implicit val arbitRaum: Arbitrary[Raum] = Arbitrary(raumGen)
}