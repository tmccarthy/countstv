package au.id.tmm.countstv.model

final case class AllocationAfterIneligibles[C](
                                                candidateStatuses: CandidateStatuses[C],
                                                candidateVoteCounts: CandidateVoteCounts[C],
                                                transfersDueToIneligibles: Map[C, CandidateVoteCounts[C]],
                                              )
