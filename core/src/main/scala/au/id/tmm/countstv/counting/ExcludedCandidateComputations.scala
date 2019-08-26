package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts}
import au.id.tmm.probabilitymeasure.{ProbabilityMeasure, TieSensitiveSorting}

object ExcludedCandidateComputations {

  /**
    * Computes the next candidate to be excluded. This will be the remaining candidate with the least votes according to
    * [[au.id.tmm.countstv.counting.CandidateVoteCountOrdering CandidateVoteCountOrdering]].
    */
  def computeExcluded[C](
    currentCandidateVoteCounts: CandidateVoteCounts[C],
    previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
    candidateStatuses: CandidateStatuses[C],
  ): ProbabilityMeasure[C] = {
    val ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)

    TieSensitiveSorting
      .min(candidateStatuses.remainingCandidates)(ordering)
      .getOrElse(throw new IllegalArgumentException("No remaining candidates"))
  }

}
