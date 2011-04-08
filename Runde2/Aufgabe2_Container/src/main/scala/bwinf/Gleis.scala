package de.voodle.tim.bwinf.container

class Gleis(initCon: Seq[Int]) {
  val length = initCon.length
  private val con = Seq(initCon: _*).toArray
  private val wag = new Array[Int](length)

  private def arrTake(arr: Array[Int])(i: Int): Int = {
    val res = arr(i-1)
    arr(i-1) = 0
    res
  }
  private def arrPut(arr: Array[Int])(map: (Int, Int)) = map match {
    case (i, what) =>
      require(arr(i-1) == 0, "arr(i-1) at " + i + " must be 0, but is " + arr(i-1))
      arr(i-1) = what
  }

  def takeWag(i: Int) = arrTake(wag)(i)
  def takeCon(i: Int) = arrTake(con)(i)
  def putWag(map: (Int, Int)) = arrPut(wag)(map)
  def putCon(map: (Int, Int)) = arrPut(con)(map)

  private def arrString(arr: Array[Int]) = // Only print first 100
    arr take 100 map (i => if(i == 0) "_" else i.toString) mkString " "
  override def toString =
     "Container: " + arrString(con) + "\n" +
     "Waggons:   " + arrString(wag)

  // Immutable Vector copies!
  def container = Vector(con: _*)
  def waggons = Vector(wag: _*)
}
