package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.model.countsteps.AllocationAfterIneligibles.TransfersDueToIneligibles
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
                                                transfersDueToIneligibles: TransfersDueToIneligibles[C],
                                              ) extends CountStep[C] {
  override def count: Count = Count.ofIneligibleCandidateHandling
}

object AllocationAfterIneligibles {
  type TransfersDueToIneligibles[C] = Map[C, CandidateVoteCounts[C]]
}

sealed trait DistributionPhaseCountStep[C] extends CountStep[C]

/**
  * A normal count step, where papers are distributed away from an elected or ineligible candidate.
  */
final case class DistributionCountStep[C](
                                           count: Count,
                                           candidateStatuses: CandidateStatuses[C],
                                           candidateVoteCounts: CandidateVoteCounts[C],
                                           distributionSource: DistributionCountStep.Source[C],
                                         ) extends DistributionPhaseCountStep[C]

/**
  * A count step during the distribution phase where no distribution happens because a candidate was excluded with no
  * votes.
  */
final case class ExcludedNoVotesCountStep[C](
                                              count: Count,
                                              candidateStatuses: CandidateStatuses[C],
                                              candidateVoteCounts: CandidateVoteCounts[C],
                                              excludedCandidate: C,
                                            ) extends DistributionPhaseCountStep[C]

/**
  * A count step during the distribution phase where no distribution happens because a candidate was elected with no
  * surplus.
  */
final case class ElectedNoSurplusCountStep[C](
                                             count: Count,
                                             candidateStatuses: CandidateStatuses[C],
                                             candidateVoteCounts: CandidateVoteCounts[C],
                                             electedCandidate: C,
                                             ) extends DistributionPhaseCountStep[C]

/**
  * A count step where candidates have been elected according to final election conditions.
  */
final case class FinalElectionCountStep[C](
                                            count: Count,
                                            candidateStatuses: CandidateStatuses[C],
                                            candidateVoteCounts: CandidateVoteCounts[C],
                                          ) extends CountStep[C]

object DistributionCountStep {

  final case class Source[C](
                              candidate: C,
                              candidateDistributionReason: CandidateDistributionReason,
                              sourceCounts: Set[Count],
                              transferValue: TransferValue,
                            )

}
