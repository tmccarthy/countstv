package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.Count
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts}

final case class AllocationAfterIneligibles[C](
                                                candidateStatuses: CandidateStatuses[C],
                                                candidateVoteCounts: CandidateVoteCounts[C],
                                                transfersDueToIneligibles: Map[C, CandidateVoteCounts[C]],
                                              ) extends CountStep[C] {
  override def count: Count = 1
}
