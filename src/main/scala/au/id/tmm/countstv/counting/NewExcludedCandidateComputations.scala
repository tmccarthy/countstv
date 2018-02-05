package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, ProbabilityMeasure}

object NewExcludedCandidateComputations {
  def computeExcluded[C](
                          currentCandidateVoteCounts: CandidateVoteCounts[C],
                          previousCandidateVoteCounts: Stream[CandidateVoteCounts[C]],
                          candidateStatuses: CandidateStatuses[C],
                        ): ProbabilityMeasure[C] = {
    val ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCounts)

    NewTieSensitiveSorting.min(candidateStatuses.remainingCandidates)(ordering)
      .getOrElse(throw new IllegalArgumentException("No remaining candidates"))
  }

}
