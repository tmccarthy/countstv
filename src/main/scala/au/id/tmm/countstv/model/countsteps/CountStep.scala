package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.model.values.{Count, TransferValue}
import au.id.tmm.countstv.model.{CandidateDistributionReason, CandidateStatus, CandidateStatuses, CandidateVoteCounts}

/**
  * A representation of the state of a count after a step.
  */
sealed trait CountStep[C] {

  def count: Count

  def candidateStatuses: CandidateStatuses[C]

  def candidateVoteCounts: CandidateVoteCounts[C]

}

/**
  * The first count step in any count, indicating the allocation of papers to their first preference, regardless of the
  * candidate's eligibility.
  */
final case class InitialAllocation[C](
                                       candidateStatuses: CandidateStatuses[C],
                                       candidateVoteCounts: CandidateVoteCounts[C],
                                     ) extends CountStep[C] {
  require{
    val eligibleCandidateStatuses = Set[CandidateStatus](CandidateStatus.Remaining, CandidateStatus.Ineligible)

    candidateStatuses.asMap.forall { case (candidate, candidateStatus) =>
      eligibleCandidateStatuses.contains(candidateStatus)
    }
  }

  override def count: Count = Count.ofInitialAllocation
}

/**
  * The second count step in any count, indicating the allocation of papers away from inelligible candidates.
  */
final case class AllocationAfterIneligibles[C](
                                                candidateStatuses: CandidateStatuses[C],
                                                candidateVoteCounts: CandidateVoteCounts[C],
                                                transfersDueToIneligibles: Map[C, CandidateVoteCounts[C]],
                                              ) extends CountStep[C] {
  override def count: Count = Count.ofIneligibleCandidateHandling
}

/**
  * A normal count step, where papers are distributed away from an elected or ineligible candidate.
  */
final case class DistributionCountStep[C] (
                                            count: Count,
                                            candidateStatuses: CandidateStatuses[C],
                                            candidateVoteCounts: CandidateVoteCounts[C],
                                            distributionSource: Option[DistributionCountStep.Source[C]],
                                          ) extends CountStep[C] {

}

object DistributionCountStep {

  final case class Source[C](
                              candidate: C,
                              candidateDistributionReason: CandidateDistributionReason,
                              sourceCounts: Set[Count],
                              transferValue: TransferValue,
                            )

}
