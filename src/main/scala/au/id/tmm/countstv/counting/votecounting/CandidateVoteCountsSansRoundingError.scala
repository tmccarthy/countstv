package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.model.VoteCount

// TODO maybe use this to express transfers from ineligible candidates?
private[votecounting] final case class CandidateVoteCountsSansRoundingError[C](
                                                                                perCandidate: Map[C, VoteCount],
                                                                                exhausted: VoteCount,
                                                                              )
