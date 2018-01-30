package au.id.tmm.countstv.model
import au.id.tmm.countstv.Count

final case class AllocationAfterIneligibles[C](
                                                candidateStatuses: CandidateStatuses[C],
                                                candidateVoteCounts: CandidateVoteCounts[C],
                                                transfersDueToIneligibles: Map[C, CandidateVoteCounts[C]],
                                              ) extends CountStep[C] {
  override def count: Count = 1
}
