package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.PreferenceTree.PreferenceTreeNode

private[counting] final case class PaperBundle[C](
                                                   transferValue: Double,
                                                   preferenceTreeNode: PreferenceTreeNode[C],
                                                   origin: PaperBundle.Origin[C],
                                                 ) {
  def associatedCandidate: C = preferenceTreeNode.associatedCandidate
}

object PaperBundle {

  sealed trait Origin[+C]

  object Origin {
    case object InitialAllocation extends Origin[Nothing]
    final case class IneligibleCandidate[C](source: C) extends Origin[C]
    final case class ElectedCandidate[C](source: C, transferValue: Double) extends Origin[C]
    final case class ExcludedCandidate[C](source: C) extends Origin[C]
  }

}