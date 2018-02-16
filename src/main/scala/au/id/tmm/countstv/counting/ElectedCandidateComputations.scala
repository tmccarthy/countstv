package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.values.NumVotes
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, ProbabilityMeasure}
import au.id.tmm.utilities.collection.DupelessSeq

object ElectedCandidateComputations {

  def newlyExceedingQuota[C](
                              currentCandidateVoteCounts: CandidateVoteCounts[C],
                              previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
                              candidateStatuses: CandidateStatuses[C],
                              numVacancies: Int,
                              quota: NumVotes,
                            ): ProbabilityMeasure[DupelessSeq[C]] = {

    val unelectedCandidatesExceedingQuota = candidateStatuses.remainingCandidates
      .toStream
      .filter { candidate =>
        currentCandidateVoteCounts.perCandidate(candidate).numVotes >= quota
      }

    val ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)

    TieSensitiveSorting.sort[C](unelectedCandidatesExceedingQuota)(ordering)
      .map(_.reverse.to[DupelessSeq])
  }

  def finallyElected[C](
                         currentCandidateVoteCounts: CandidateVoteCounts[C],
                         previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
                         candidateStatuses: CandidateStatuses[C],
                         numVacancies: Int,
                         quota: NumVotes,
                       ): ProbabilityMeasure[DupelessSeq[C]] = {

    val numUnfilledVacancies = numVacancies - candidateStatuses.electedCandidates.size

    val ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)

    if (numUnfilledVacancies == 1 && candidateStatuses.remainingCandidates.size == 2) {
      TieSensitiveSorting.sort(candidateStatuses.remainingCandidates)(ordering)
        .map(_.lastOption.to[DupelessSeq])

    } else if (numUnfilledVacancies == candidateStatuses.remainingCandidates.size) {
      TieSensitiveSorting.sort(candidateStatuses.remainingCandidates)(ordering)
        .map(_.reverse.to[DupelessSeq])

    } else {
      ProbabilityMeasure.always(DupelessSeq.empty)

    }
  }
}
