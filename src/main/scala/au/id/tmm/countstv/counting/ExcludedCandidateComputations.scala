package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, ProbabilityMeasure}

@deprecated(message = "This is not right")
object ExcludedCandidateComputations {
  def computeExcluded[C](
                          counts: CandidateVoteCounts[C],
                          candidateStatuses: CandidateStatuses[C],
                        ): ProbabilityMeasure[C] = {
    require(candidateStatuses.remainingCandidates.nonEmpty)

    TieSensitiveSorting.minBy(candidateStatuses.remainingCandidates)(counts.perCandidate(_).numVotes)
      .get
  }

}
