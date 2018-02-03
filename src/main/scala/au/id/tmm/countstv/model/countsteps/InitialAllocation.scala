package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.Count
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, CandidateVoteCounts}

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

  override def count: Count = 0
}
