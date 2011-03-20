object Transposition {
  type Trans = List[Int]
  type Transs = List[Trans]

  def trans(perm: Seq[Int]) = transpositions(perm) //filterNot (_.tail.isEmpty)

  private def transpositions(perm: Seq[Int], handled: Set[Int] = Set(), start: Int = 1): Transs =
    if(start > perm.length || start < 1) Nil
    else {
      val trans = transposition(perm, start)
      val newHandled = handled ++ trans
      val next = (start to perm.length) findIndexOf (i => !(newHandled contains i))
      if(next >= 0)
        trans :: transpositions(perm, newHandled, next+start)
      else
        trans :: Nil
    }
  private def transposition(perm: Seq[Int], start: Int) = {
    val startIdx = start - 1 // from 1...n -> 0..n
    val transPerm = perm map (_ - 1)
    (startIdx :: helper(transPerm, startIdx)(transPerm(startIdx))) map (_ + 1) // from 0..n -> 1...n
  }
  private def helper(perm: Seq[Int], start: Int)(idx: Int): Trans =
    if(start == idx) Nil
    else (idx) :: helper(perm,start)(perm(idx))
}
