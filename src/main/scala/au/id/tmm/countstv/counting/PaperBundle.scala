package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.PaperBundle.Origin
import au.id.tmm.countstv.model.PreferenceTree.PreferenceTreeNode

private[counting] final case class PaperBundle[C](
                                                   transferValue: Double,
                                                   preferenceTreeNode: PreferenceTreeNode[C],
                                                   origin: PaperBundle.Origin[C],
                                                 ) {
  def associatedCandidate: C = preferenceTreeNode.associatedCandidate

  def distributionGivenIneligibles(ineligibleCandidates: Set[C]): Set[PaperBundle[C]] = {
    if (ineligibleCandidates contains associatedCandidate) {

      val originForDistributedBundles = Origin.IneligibleCandidate(associatedCandidate)

      val nodesForDistributedBundles = childNodesNotAssignedTo(preferenceTreeNode, ineligibleCandidates)

      nodesForDistributedBundles
        .map(childNode => PaperBundle(this.transferValue, childNode, originForDistributedBundles))
        .toSet
    } else {
      Set(this)
    }
  }

  private def childNodesNotAssignedTo(
                                       rootNode: PreferenceTreeNode[C],
                                       candidatesToAvoid: Set[C],
                                     ): Iterator[PreferenceTreeNode[C]] = {
    rootNode.children.valuesIterator.flatMap { childNode =>
      if (candidatesToAvoid contains childNode.associatedCandidate) {
        childNodesNotAssignedTo(childNode, candidatesToAvoid)
      } else {
        Set(childNode)
      }
    }
  }
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