package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.PaperBundle.Origin
import au.id.tmm.countstv.model.CandidateStatuses
import au.id.tmm.countstv.model.PreferenceTree.PreferenceTreeNode

private[counting] final case class PaperBundle[C](
                                                   transferValue: Double,
                                                   preferenceTreeNode: PreferenceTreeNode[C],
                                                   origin: PaperBundle.Origin[C],
                                                 ) {
  def associatedCandidate: C = preferenceTreeNode.associatedCandidate

  def distributeToRemainingCandidates(
                                       origin: Origin[C],
                                       candidateStatuses: CandidateStatuses[C],
                                     ): Set[PaperBundle[C]] = {
    if (candidateStatuses.remaining contains associatedCandidate) {
      Set(this)
    } else {
      val nodesForDistributedBundles = childNodesAssignedToRemainingCandidates(
        preferenceTreeNode,
        candidateStatuses.remaining,
      )

      nodesForDistributedBundles
        .map(childNode => PaperBundle(this.transferValue, childNode, origin))
        .toSet
    }
  }

  private def childNodesAssignedToRemainingCandidates(
                                                       rootNode: PreferenceTreeNode[C],
                                                       remainingCandidates: Set[C],
                                                     ): Iterator[PreferenceTreeNode[C]] = {
    rootNode.children.valuesIterator.flatMap { childNode =>
      if (remainingCandidates contains childNode.associatedCandidate) {
        Set(childNode)
      } else {
        childNodesAssignedToRemainingCandidates(childNode, remainingCandidates)
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