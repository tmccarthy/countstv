package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, CandidateVoteCounts, VoteCount}

object VoteCountingUtilities {

  private[votecounting] def incorporateElectedCandidatesIntoCount[C](
                                                                      quota: NumVotes,
                                                                      simpleCount: CandidateVoteCountsSansRoundingError[C],
                                                                      candidateStatuses: CandidateStatuses[C],
                                                                    ): CandidateVoteCountsSansRoundingError[C] = {
    simpleCount.copy(
      perCandidate = simpleCount.perCandidate.map { case (candidate, voteCountFromSimpleCount) =>

        val candidateStatus = candidateStatuses.asMap(candidate)

        val voteCountForCandidate = {
          if (candidateStatus.isInstanceOf[CandidateStatus.Elected] && voteCountFromSimpleCount == VoteCount.zero) {
            voteCountFromSimpleCount + VoteCount(NumPapers(0), quota)
          } else {
            voteCountFromSimpleCount
          }
        }

        candidate -> voteCountForCandidate
      }
    )
  }

  private[votecounting] def updateRoundingError[C](
                                                    initialNumPapers: NumPapers,
                                                    voteCounts: CandidateVoteCountsSansRoundingError[C],
                                                  ): CandidateVoteCounts[C] = {
    val initialVoteCount = VoteCount(initialNumPapers.asLong)
    val roundingError = initialVoteCount - voteCounts.total

    voteCounts.withRoundingError(
      roundingError = roundingError
    )
  }

}
