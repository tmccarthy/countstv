package au.id.tmm.countstv.model

final case class CandidateVoteCounts[C](
                                         perCandidate: Map[C, VoteCount],
                                         exhausted: VoteCount,
                                         roundingError: VoteCount,
                                       )
