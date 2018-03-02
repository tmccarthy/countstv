package au.id.tmm.countstv.model

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.model.PaperBundle.Origin
import au.id.tmm.countstv.model.PreferenceTree.PreferenceTreeNode
import au.id.tmm.countstv.model.values.{Count, NumPapers, TransferValue, TransferValueCoefficient}

sealed trait PaperBundle[C] {

  def assignedCandidate: Option[C]

  def origin: Origin[C]

  def distributeToRemainingCandidates(
                                       origin: Origin[C],
                                       candidateStatuses: CandidateStatuses[C],
                                     ): PaperBundles[C] =
    PaperBundle.distributeIfCandidateNotRemaining(this, origin, candidateStatuses)

  def numPapers: NumPapers

  def transferValue: TransferValue

}

final case class RootPaperBundle[C](preferenceTree: PreferenceTree[C]) extends PaperBundle[C] {
  override def assignedCandidate: Option[C] = None

  override def origin: Origin[C] = PaperBundle.Origin.InitialAllocation

  override def numPapers: NumPapers = preferenceTree.numPapers

  override def transferValue: TransferValue = TransferValue(1.0d)

  def distribute: PaperBundles[C] = {
    val childBundles = preferenceTree.children.valuesIterator.map { childNode =>
      AssignedPaperBundle(
        transferValue = TransferValue(1.0d),
        preferenceTreeNode = childNode,
        origin = PaperBundle.Origin.InitialAllocation,
      )
    }

    childBundles.toSet[PaperBundle[C]]
  }
}

final case class AssignedPaperBundle[C](
                                         transferValue: TransferValue,
                                         preferenceTreeNode: PreferenceTreeNode[C],
                                         origin: PaperBundle.Origin[C]
                                       ) extends PaperBundle[C] {

  override def assignedCandidate: Option[C] = Some(preferenceTreeNode.associatedCandidate)

  override def numPapers: NumPapers = preferenceTreeNode.numPapers

  override val hashCode: Int = preferenceTreeNode.hashCode()
}

// TODO track count too
final case class ExhaustedPaperBundle[C](
                                          numPapers: NumPapers,
                                          transferValue: TransferValue,
                                          origin: Origin[C],
                                        ) extends PaperBundle[C] {
  override def assignedCandidate: Option[C] = None
}

object PaperBundle {

  def rootBundleFor[C](preferenceTree: PreferenceTree[C]): RootPaperBundle[C] = RootPaperBundle[C](preferenceTree)

  def distributeIfCandidateNotRemaining[C](
                                            bundle: PaperBundle[C],
                                            origin: Origin[C],
                                            candidateStatuses: CandidateStatuses[C],
                                          ): PaperBundles[C] = {

    bundle match {
      case b: ExhaustedPaperBundle[C] => Set[PaperBundle[C]](b)
      case b if b.assignedCandidate.exists(candidateStatuses.remainingCandidates.contains) => Set[PaperBundle[C]](b)
      case b: AssignedPaperBundle[C] =>
        val nodesForDistributedBundles = childNodesAssignedToRemainingCandidates(
          b.preferenceTreeNode,
          candidateStatuses.remainingCandidates,
        )

        distributeToRemainingCandidates(b, origin, nodesForDistributedBundles)
      case b: RootPaperBundle[C] =>
        val nodesForDistributedBundles = b.preferenceTree.children.valuesIterator

        distributeToRemainingCandidates(b, origin, nodesForDistributedBundles)
    }
  }

  private def distributeToRemainingCandidates[C](
                                                  bundle: PaperBundle[C],
                                                  origin: Origin[C],
                                                  nodesForDistributedBundles: Iterator[PreferenceTreeNode[C]],
                                                ): PaperBundles[C] = {

    val distributedTransferValue = origin match {
      case PaperBundle.Origin.ElectedCandidate(_, appliedTransferValue, _) =>
        appliedTransferValue * bundle.transferValue
      case _ => bundle.transferValue
    }

    val bundlesDistributedToCandidates = nodesForDistributedBundles
      .map(childNode => AssignedPaperBundle(distributedTransferValue, childNode, origin))
      .toSet

    val exhaustedPaperBundle = {
      val numPapersDistributedToCandidates = bundlesDistributedToCandidates
        .toStream
        .map(_.numPapers)
        .foldLeft(NumPapers(0))(_ + _)

      val numExhaustedPapers = bundle.numPapers - numPapersDistributedToCandidates

      if (numExhaustedPapers > NumPapers(0)) {
        Some(ExhaustedPaperBundle(
          numPapers = numExhaustedPapers,
          transferValue = distributedTransferValue,
          origin = origin,
        ))
      } else {
        None
      }
    }

    bundlesDistributedToCandidates ++ exhaustedPaperBundle
  }

  private def childNodesAssignedToRemainingCandidates[C](
                                                          rootNode: PreferenceTree[C],
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

  sealed trait Origin[+C] {
    def count: Count
  }

  object Origin {
    case object InitialAllocation extends Origin[Nothing] {
      def count: Count = Count.ofInitialAllocation
    }

    final case class IneligibleCandidate[C](source: C) extends Origin[C] {
      def count: Count = Count.ofIneligibleCandidateHandling
    }

    final case class ElectedCandidate[C](source: C, transferValue: TransferValueCoefficient, count: Count) extends Origin[C]

    final case class ExcludedCandidate[C](source: C, count: Count) extends Origin[C]

  }

}