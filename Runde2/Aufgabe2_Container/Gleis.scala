class Gleis(container: Seq[Int]) {
  val length = container.length
  private val con = container.toArray
  private val wag = new Array[Int](length)

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
  def take(i: Int) = arrGet(con, true)(i) orElse arrGet(wag, true)(i)

  def put(i: Int)(what: Int) = arrPut(wag)(i)(what) orElse arrPut(con)(i)(what)

  private def arrString(arr: Array[Int]) = arr map (i => if(i == 0) "_" else i.toString) mkString " "
  override def toString =
     "Container: " + arrString(con) + "\n" +
     "Waggons:   " + arrString(wag)
}
