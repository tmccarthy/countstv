package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.PaperBundle.Origin
import au.id.tmm.countstv.model.PreferenceTree.PreferenceTreeNode
import au.id.tmm.countstv.model.{CandidateStatuses, PreferenceTree}

import scala.collection.immutable.{Bag, HashBag, HashedBagConfiguration}

private[counting] sealed trait PaperBundle[C] {

  def assignedCandiate: Option[C]

  def origin: Origin[C]

  def distributeToRemainingCandidates(
                                       origin: Origin[C],
                                       candidateStatuses: CandidateStatuses[C],
                                     ): Bag[PaperBundle[C]]

  def numPapers: Long

  def transferValue: Double

}

private[counting] final case class AssignedPaperBundle[C](
                                                           transferValue: Double,
                                                           preferenceTreeNode: PreferenceTreeNode[C],
                                                           origin: PaperBundle.Origin[C]
                                                         ) extends PaperBundle[C] {

  override def assignedCandiate: Option[C] = Some(preferenceTreeNode.associatedCandidate)

  override def numPapers: Long = preferenceTreeNode.numPapers

  override def distributeToRemainingCandidates(
                                                origin: Origin[C],
                                                candidateStatuses: CandidateStatuses[C],
                                              ): Bag[PaperBundle[C]] = {
    if (candidateStatuses.remaining contains this.preferenceTreeNode.associatedCandidate) {
      Bag[PaperBundle[C]](this)
    } else {
      val nodesForDistributedBundles = childNodesAssignedToRemainingCandidates(
        this.preferenceTreeNode,
        candidateStatuses.remaining,
      )

      val distributedTransferValue = origin match {
        case PaperBundle.Origin.ElectedCandidate(_, appliedTransferValue) =>
          this.transferValue * appliedTransferValue
        case _ => this.transferValue
      }

      val bundlesDistributedToCandidates = nodesForDistributedBundles
        .map(childNode => AssignedPaperBundle(distributedTransferValue, childNode, origin))
        .toSet

      val exhaustedPaperBundle = {
        val numPapersDistributedToCandidates = bundlesDistributedToCandidates
          .toStream
          .map(_.numPapers)
          .sum

        val numExhaustedPapers = this.numPapers - numPapersDistributedToCandidates

        if (numExhaustedPapers > 0) {
          Some(ExhaustedPaperBundle(
            numPapers = numExhaustedPapers,
            transferValue = distributedTransferValue,
            origin = origin,
          ))
        } else {
          None
        }
      }

      Bag[PaperBundle[C]]() ++ bundlesDistributedToCandidates ++ exhaustedPaperBundle
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

private[counting] final case class ExhaustedPaperBundle[C](
                                                            numPapers: Long,
                                                            transferValue: Double,
                                                            origin: Origin[C],
                                                          ) extends PaperBundle[C] {
  override def assignedCandiate: Option[C] = None

  override def distributeToRemainingCandidates(
                                                origin: Origin[C],
                                                candidateStatuses: CandidateStatuses[C],
                                              ): Bag[PaperBundle[C]] = Bag(this)
}

object PaperBundle {

  implicit def bagConfiguration[C]: HashedBagConfiguration[PaperBundle[C]] =
    HashedBagConfiguration.keepAll[PaperBundle[C]]

  def initialBundlesFor[C](testPreferenceTree: PreferenceTree[C]): Bag[PaperBundle[C]] =
    testPreferenceTree.children.valuesIterator.map { preferenceTreeNode =>
      AssignedPaperBundle(
        transferValue = 1d,
        preferenceTreeNode = preferenceTreeNode,
        origin = Origin.InitialAllocation,
      ).asInstanceOf[PaperBundle[C]]
    }
      .to[Bag](HashBag.canBuildFrom(PaperBundle.bagConfiguration))

  sealed trait Origin[+C]

  object Origin {
    case object InitialAllocation extends Origin[Nothing]
    final case class IneligibleCandidate[C](source: C) extends Origin[C]
    final case class ElectedCandidate[C](source: C, transferValue: Double) extends Origin[C]
    final case class ExcludedCandidate[C](source: C) extends Origin[C]
  }

}