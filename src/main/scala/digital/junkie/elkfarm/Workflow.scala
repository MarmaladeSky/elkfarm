package digital.junkie.elkfarm

sealed trait Workflow {
  def title: String
  def description: String
}

object Workflow {
  case object Simple extends Workflow {
    val title: String       = "Simple"
    val description: String = "A simple alias-based indices management"
  }

  val all: Seq[Workflow] = Seq(Simple)

  /** Looks up a workflow by its title, case-insensitively. */
  def byName(name: String): Option[Workflow] =
    all.find(_.title.equalsIgnoreCase(name))
}
