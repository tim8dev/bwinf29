package de.voodle.tim.bwinf.container

class Gleis(initCon: Seq[Int]) {
  val length = initCon.length
  private val con = Seq(initCon: _*).toArray
  private val wag = new Array[Int](length)

  // Immutable Vectors!
  def container = Vector(con: _*)
  def waggons = Vector(wag: _*)

  private def arrGet(arr: Array[Int], take: Boolean)(i: Int): Option[Int] = {
    val res = arr(i-1)
    if(take)
      arr(i-1) = 0
    if (res == 0) None else Some(res)
  }
  private def arrPut(arr: Array[Int])(i: Int)(what: Int) =
    arr(i-1) match {
      case 0 =>
        arr(i-1) = what
	Some(())
      case _ => None
    }

  def apply(i: Int) = arrGet(con, false)(i) orElse arrGet(wag, false)(i)
  def take(i: Int)  = arrGet(con, true)(i) orElse arrGet(wag, true)(i)

  def put(mapping: (Int,Int)): Option[Unit] = put(mapping._1)(mapping._2)
  def put(i: Int)(what: Int) = arrPut(wag)(i)(what) orElse arrPut(con)(i)(what)

  private def arrString(arr: Array[Int]) = arr take 100 map (i => if(i == 0) "_" else i.toString) mkString " "
  override def toString =
     "Container: " + arrString(con) + "\n" +
     "Waggons:   " + arrString(wag)
}
