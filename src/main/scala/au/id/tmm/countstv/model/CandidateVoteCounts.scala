package au.id.tmm.countstv.model

/**
  * A bundle of the [[VoteCount]] values per candidate, a count of exhausted votes, and a tally of any rounding error.
  */
final case class CandidateVoteCounts[C](
                                         perCandidate: Map[C, VoteCount],
                                         exhausted: VoteCount,
                                         roundingError: VoteCount,
                                       )
