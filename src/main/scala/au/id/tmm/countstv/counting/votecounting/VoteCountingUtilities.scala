package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, VoteCount}

private[votecounting] object VoteCountingUtilities {

  def incorporateElectedCandidatesIntoCount[C](
                                                        quota: NumVotes,
                                                        simpleCount: CandidateVoteCountsSansRoundingError[C],
                                                        candidateStatuses: CandidateStatuses[C],
                                                      ): CandidateVoteCountsSansRoundingError[C] = {
    simpleCount.copy(
      perCandidate = simpleCount.perCandidate.map { case (candidate, voteCountFromSimpleCount) =>

        val candidateStatus = candidateStatuses.asMap(candidate)

        val voteCountForCandidate = {
          // TODO I don't think this is correct *during* a distribution
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

}
