package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.values.{Count, NumVotes, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, CandidateVoteCounts}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.probabilitymeasure.{ProbabilityMeasure, TieSensitiveSorting}

private[counting] object ElectedCandidateComputations {

  /**
    * Identifies any candidates that exceed quota, but have not yet been marked as elected.
    */
  def newlyExceedingQuota[C](
    currentCandidateVoteCounts: CandidateVoteCounts[C],
    previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
    candidateStatuses: CandidateStatuses[C],
    numVacancies: Int,
    quota: NumVotes,
  ): ProbabilityMeasure[DupelessSeq[C]] = {

    val unelectedCandidatesExceedingQuota = candidateStatuses.remainingCandidates
      .to(LazyList)
      .filter { candidate =>
        currentCandidateVoteCounts.perCandidate(candidate).numVotes >= quota
      }

    val ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)

    TieSensitiveSorting
      .sort[C](unelectedCandidatesExceedingQuota)(ordering)
      .map(_.reverse.to(DupelessSeq))
  }

  def newCandidateStatusesAfterElectionOf[C](
    newlyElectedCandidates: DupelessSeq[C],
    count: Count,
    oldCandidateStatuses: CandidateStatuses[C],
  ): CandidateStatuses[C] = {
    val numCandidatesPreviouslyElected = oldCandidateStatuses.electedCandidates.size

    val statusesForNewlyElectedCandidates = newlyElectedCandidates.zipWithIndex
      .map {
        case (newlyElectedCandidate, indexElectedThisStep) =>
          newlyElectedCandidate -> (numCandidatesPreviouslyElected + indexElectedThisStep)
      }
      .map {
        case (newlyElectedCandidate, ordinalElected) =>
          newlyElectedCandidate -> CandidateStatus.Elected(Ordinal(ordinalElected), count)
      }
      .toMap

    oldCandidateStatuses.updateFrom(statusesForNewlyElectedCandidates)
  }
}
