package au.id.tmm.countstv.model

final case class InitialAllocation[C](
                                       candidateStatuses: CandidateStatuses[C],
                                       candidateVoteCounts: CandidateVoteCounts[C],
                                     ) {
  require{
    val eligibleCandidateStatuses = Set[CandidateStatus](CandidateStatus.Remaining, CandidateStatus.Ineligible)

    candidateStatuses.asMap.forall { case (candidate, candidateStatus) =>
        eligibleCandidateStatuses.contains(candidateStatus)
    }
  }
}
