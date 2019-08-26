package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.counting.PaperBundles
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts}

object FullCountVoteCounting {

  /**
    * Produces a count of votes per candidate, as well as a count of exhausted votes, and any error due to rounding. Any
    * elected candidates, despite necessarily having no papers allocated to them in the count, are counted as having a
    * quota of votes.
    */
  def performFullRecount[C](
    initialNumPapers: NumPapers,
    quota: NumVotes,
    candidateStatuses: CandidateStatuses[C],
    paperBundles: PaperBundles[C],
  ): CandidateVoteCounts[C] = {

    val simpleCount = SimpleVoteCounting.performSimpleCount(candidateStatuses.allCandidates, paperBundles)

    val countIncorporatingElectedCandidates =
      VoteCountingUtilities.incorporateElectedCandidatesIntoCount(quota, simpleCount, candidateStatuses)

    VoteCountingUtilities.updateRoundingError(initialNumPapers, countIncorporatingElectedCandidates)
  }

}
