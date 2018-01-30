package au.id.tmm.countstv.model
import au.id.tmm.countstv.Count

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
