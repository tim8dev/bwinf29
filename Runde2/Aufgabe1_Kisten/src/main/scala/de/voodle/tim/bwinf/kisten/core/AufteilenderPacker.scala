package de.voodle.tim.bwinf.kisten.core

class AufteilenderPacker (kistenSeq: Seq[KisteLeer],
                          private val tf: Int = 15) extends SortierenderPacker(kistenSeq) {
  private val t = (kisten.length.toDouble / tf).ceil.toInt
  
  private val geteilteKisten: Seq[List[KisteLeer]] = {
    val startListen = Vector.fill(t)(List.empty[KisteLeer])
    ((startListen, 0) /: kisten) {
      case ((listen, idx), kiste) =>
        val liste = listen(idx)
        (listen updated (idx, kiste :: liste), (idx+1)%t)
    }._1
  }
  def min: KistenSatz =
    geteilteKisten.map(kisten => new OptimalPacker(kisten))
               .map(_.min)
               .reduceLeft(_ neben _)
}
