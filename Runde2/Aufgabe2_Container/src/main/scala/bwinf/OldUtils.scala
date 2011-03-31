object OldUtils {
  def cycle(perm: Seq[Int], start: Int): List[Int] = {
    def step(idx: Int): List[Int] = // Hilfsfunktion
      if(start == idx) Nil
      else idx :: step(perm(idx - 1))
    start :: step(perm(start-1))
  }
  def cyclesOf(perm: Seq[Int], ready: Set[Int]): List[List[Int]] = {
    (1 to perm.length) find (i => !ready.contains(i)) match { // bis zu n Aufrufe; Zugriff auf Hashset O(1)
      case Some(start) => 
	val newCycle = cycle(perm, start)
	val newReady = ready ++ newCycle // O(n)
	newCycle :: cyclesOf(perm, newReady)
      case None =>
	Nil
    }
  }
}