package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.PaperBundle.Origin
import au.id.tmm.countstv.model.PreferenceTree.PreferenceTreeNode
import au.id.tmm.countstv.model.{CandidateStatuses, PreferenceTree}

import scala.collection.immutable.{Bag, HashedBagConfiguration}

private[counting] sealed trait PaperBundle[C] {

  def assignedCandidate: Option[C]

  def origin: Origin[C]

  def distributeToRemainingCandidates(
                                       origin: Origin[C],
                                       candidateStatuses: CandidateStatuses[C],
                                     ): Bag[PaperBundle[C]] =
    PaperBundle.distributeIfCandidateNotRemaining(this, origin, candidateStatuses)

  def numPapers: Long

  def transferValue: Double

}

private[counting] final case class RootPaperBundle[C](preferenceTree: PreferenceTree[C]) extends PaperBundle[C] {
  override def assignedCandidate: Option[C] = None

  override def origin: Origin[C] = PaperBundle.Origin.InitialAllocation

  override def numPapers: Long = preferenceTree.numPapers

  override def transferValue: Double = 1.0d
}

private[counting] final case class AssignedPaperBundle[C](
                                                           transferValue: Double,
                                                           preferenceTreeNode: PreferenceTreeNode[C],
                                                           origin: PaperBundle.Origin[C]
                                                         ) extends PaperBundle[C] {

  override def assignedCandidate: Option[C] = Some(preferenceTreeNode.associatedCandidate)

  override def numPapers: Long = preferenceTreeNode.numPapers

}

private[counting] final case class ExhaustedPaperBundle[C](
                                                            numPapers: Long,
                                                            transferValue: Double,
                                                            origin: Origin[C],
                                                          ) extends PaperBundle[C] {
  override def assignedCandidate: Option[C] = None
}

object PaperBundle {

  implicit def bagConfiguration[C]: HashedBagConfiguration[PaperBundle[C]] =
    HashedBagConfiguration.keepAll[PaperBundle[C]]

  def rootBundleFor[C](preferenceTree: PreferenceTree[C]): RootPaperBundle[C] = RootPaperBundle[C](preferenceTree)

  private[counting] def distributeIfCandidateNotRemaining[C](
                                                              bundle: PaperBundle[C],
                                                              origin: Origin[C],
                                                              candidateStatuses: CandidateStatuses[C],
                                                            ): Bag[PaperBundle[C]] = {

    bundle match {
      case b: ExhaustedPaperBundle[C] => Bag[PaperBundle[C]](b)
      case b if b.assignedCandidate.exists(candidateStatuses.remainingCandidates.contains) => Bag[PaperBundle[C]](b)
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
                                                ): Bag[PaperBundle[C]] = {

    val distributedTransferValue = origin match {
      case PaperBundle.Origin.ElectedCandidate(_, appliedTransferValue) =>
        bundle.transferValue * appliedTransferValue
      case _ => bundle.transferValue
    }

    val bundlesDistributedToCandidates = nodesForDistributedBundles
      .map(childNode => AssignedPaperBundle(distributedTransferValue, childNode, origin))
      .toSet

    val exhaustedPaperBundle = {
      val numPapersDistributedToCandidates = bundlesDistributedToCandidates
        .toStream
        .map(_.numPapers)
        .sum

      val numExhaustedPapers = bundle.numPapers - numPapersDistributedToCandidates

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

  sealed trait Origin[+C]

  object Origin {
    case object InitialAllocation extends Origin[Nothing]
    final case class IneligibleCandidate[C](source: C) extends Origin[C]
    final case class ElectedCandidate[C](source: C, transferValue: Double) extends Origin[C]
    final case class ExcludedCandidate[C](source: C) extends Origin[C]
  }

}