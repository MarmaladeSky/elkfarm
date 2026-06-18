package digital.junkie.elkfarm

case class ManagedIndex(
    name: String,
    currentIndex: Int,
    existingIndices: Seq[Int]
) {

  def currentIndexName: String = s"${name}_v$currentIndex"
  
  def nextIndexVersion: Int = existingIndices.toSet.+(currentIndex).max + 1

}
