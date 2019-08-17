package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.PaperBundle.Origin
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.preferences.PreferenceTree
import au.id.tmm.countstv.model.preferences.PreferenceTree.PreferenceTreeNode
import au.id.tmm.countstv.model.values.{Count, NumPapers, TransferValue}

import scala.collection.parallel.immutable.ParSet

/**
  * A representation of a bundle of ballot papers, along with their origin in the count, their assigned candidate and
  * their transfer value.
  */
private[counting] sealed trait PaperBundle[C] {

  def assignedCandidate: Option[C]

  def origin: Origin[C]

  def distributeToRemainingCandidates(
                                       origin: Origin[C],
                                       count: Count,
                                       candidateStatuses: CandidateStatuses[C],
                                     ): PaperBundles[C] =
    PaperBundle.distributeIfCandidateNotRemaining(this, origin, count, candidateStatuses)

  def numPapers: NumPapers

  def transferValue: TransferValue

}

/**
  * The root paper bundle, representing all ballot papers in a count. This is essentially a pointer to the root node of
  * a `PreferenceTree`, along with the functionality to distribute these papers.
  */
private[counting] final case class RootPaperBundle[C](preferenceTree: PreferenceTree[C]) extends PaperBundle[C] {
  override def assignedCandidate: Option[C] = None

  override def origin: Origin[C] = PaperBundle.Origin.InitialAllocation

  override def numPapers: NumPapers = preferenceTree.numPapers

  override def transferValue: TransferValue = TransferValue(1.0d)

  def distribute: PaperBundles[C] = {
    val childBundles = preferenceTree.children.map { childNode =>
      AssignedPaperBundle(
        transferValue = TransferValue(1.0d),
        preferenceTreeNode = childNode,
        origin = PaperBundle.Origin.InitialAllocation,
      )
    }

    val builder = ParSet.newCombiner[PaperBundle[C]]

    builder ++= childBundles

    builder.result()
  }
}

/**
  * A bundle of papers assigned to a candidate in the count. This is essentially a pointer to a `PreferenceTreeNode`,
  * along with the origin of the papers in the count and their transfer value.
  */
private[counting] final case class AssignedPaperBundle[C](
                                                           transferValue: TransferValue,
                                                           preferenceTreeNode: PreferenceTreeNode[C],
                                                           origin: PaperBundle.Origin[C]
                                                         ) extends PaperBundle[C] {

  override def assignedCandidate: Option[C] = Some(preferenceTreeNode.associatedCandidate)

  override def numPapers: NumPapers = preferenceTreeNode.numPapers

  override val hashCode: Int = preferenceTreeNode.hashCode()
}

/**
  * A bundle of exhausted papers. Because exhausted papers are no longer tracked by a `PreferenceTree`, this
  * representation is just number of papers and a transfer value.
  */
private[counting] final case class ExhaustedPaperBundle[C](
                                                            numPapers: NumPapers,
                                                            transferValue: TransferValue,
                                                            origin: Origin[C],
                                                            exhaustedAtCount: Count,
                                                            originatingNode: PreferenceTree[C],
                                                          ) extends PaperBundle[C] {
  override def assignedCandidate: Option[C] = None
}

private[counting] object PaperBundle {

  def rootBundleFor[C](preferenceTree: PreferenceTree[C]): RootPaperBundle[C] = RootPaperBundle[C](preferenceTree)

  private def distributeIfCandidateNotRemaining[C](
                                                    bundle: PaperBundle[C],
                                                    origin: Origin[C],
                                                    count: Count,
                                                    candidateStatuses: CandidateStatuses[C],
                                                  ): PaperBundles[C] = {

    bundle match {
      case b: ExhaustedPaperBundle[C] => ParSet[PaperBundle[C]](b)
      case b if b.assignedCandidate.exists(candidateStatuses.remainingCandidates.contains) => ParSet[PaperBundle[C]](b)
      case b: AssignedPaperBundle[C] =>
        val nodesForDistributedBundles = childNodesAssignedToRemainingCandidates(
          b.preferenceTreeNode,
          candidateStatuses.remainingCandidates,
        )

        distributeToRemainingCandidates(b, origin, count, nodesForDistributedBundles)
      case b: RootPaperBundle[C] =>
        val nodesForDistributedBundles = b.preferenceTree.children

        distributeToRemainingCandidates(b, origin, count, nodesForDistributedBundles)
    }
  }

  private def distributeToRemainingCandidates[C](
                                                  bundle: PaperBundle[C],
                                                  origin: Origin[C],
                                                  count: Count,
                                                  nodesForDistributedBundles: List[PreferenceTreeNode[C]],
                                                ): PaperBundles[C] = {

    val distributedTransferValue = origin match {
      case PaperBundle.Origin.ElectedCandidate(_, appliedTransferValue, _) =>
        appliedTransferValue
      case _ => bundle.transferValue
    }

    val bundlesDistributedToCandidates = nodesForDistributedBundles
      .map(childNode => AssignedPaperBundle(distributedTransferValue, childNode, origin))
      .to[ParSet]

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
          exhaustedAtCount = count,
          originatingNode = bundle match {
            case RootPaperBundle(preferenceTree) => preferenceTree
            case AssignedPaperBundle(_, preferenceTreeNode, _) => preferenceTreeNode
            case ExhaustedPaperBundle(_, _, _, _, originatingNode) => originatingNode
          },
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
                                                        ): List[PreferenceTreeNode[C]] = {
    rootNode.children.flatMap { childNode =>
      if (remainingCandidates contains childNode.associatedCandidate) {
        Set(childNode)
      } else {
        childNodesAssignedToRemainingCandidates(childNode, remainingCandidates)
      }
    }
  }

  /**
    * The origin of a PaperBundle
    */
  sealed trait Origin[+C] {
    def count: Count
  }

  object Origin {

    /**
      * The origin of a paper bundle that was allocated in the initial allocation.
      */
    case object InitialAllocation extends Origin[Nothing] {
      def count: Count = Count.ofInitialAllocation
    }

    /**
      * The origin of a paper bundle that was distributed away from an ineligible candidate.
      */
    final case class IneligibleCandidate[C](source: C) extends Origin[C] {
      def count: Count = Count.ofIneligibleCandidateHandling
    }

    /**
      * The origin of a paper bundle that was distributed away from an elected candidate.
      */
    final case class ElectedCandidate[C](source: C, transferValue: TransferValue, count: Count) extends Origin[C]

    /**
      * The origin of a paper bundle that was distributed away from an excluded candidate.
      */
    final case class ExcludedCandidate[C](source: C, count: Count) extends Origin[C]

  }

}