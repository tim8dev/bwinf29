package de.voodle.tim.bwinf.container

class Gleis(initCon: Seq[Int]) {
  val length = initCon.length
  protected val con = Seq(initCon: _*).toArray
  protected val wag = new Array[Int](length)

  // Immutable Vectors!
  def container = Vector(con: _*)
  def waggons = Vector(wag: _*)

  protected def arrGet(arr: Array[Int], take: Boolean)(i: Int): Option[Int] = {
    val res = arr(i-1)
    if(take)
      arr(i-1) = 0
    if (res == 0) None else Some(res)
  }
  protected def arrPut(arr: Array[Int])(i: Int)(what: Int) =
    arr(i-1) match {
      case 0 =>
        arr(i-1) = what
	      Some(())
      case _ => None
    }

  def takeWag(i: Int) = arrGet(wag, true)(i)
  def takeCon(i: Int) = arrGet(con, true)(i)
  def putWag(map: (Int, Int)) = map match {
    case (i, what) => arrPut(wag)(i)(what)
  }
  def putCon(map: (Int, Int)) = map match {
    case (i, what) => arrPut(con)(i)(what)
  }

  //def apply(i: Int) = arrGet(con, false)(i) orElse arrGet(wag, false)(i)
  def take(i: Int)  = arrGet(con, true)(i) orElse arrGet(wag, true)(i)

  def put(map: (Int, Int)): Option[Unit] = map match {
    case (i, what) => put(i)(what)
  }
  def put(i: Int)(what: Int) = arrPut(wag)(i)(what) orElse arrPut(con)(i)(what)

  private def arrString(arr: Array[Int]) = arr take 100 map (i => if(i == 0) "_" else i.toString) mkString " "
  override def toString =
     "Container: " + arrString(con) + "\n" +
     "Waggons:   " + arrString(wag)
}
