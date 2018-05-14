package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.values.{Count, NumVotes, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, CandidateVoteCounts}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.probabilities.{ProbabilityMeasure, TieSensitiveSorting}

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
      .toStream
      .filter { candidate =>
        currentCandidateVoteCounts.perCandidate(candidate).numVotes >= quota
      }

    val ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)

    TieSensitiveSorting.sort[C](unelectedCandidatesExceedingQuota)(ordering)
      .map(_.reverse.to[DupelessSeq])
  }

  /**
    * Identifies any candidates elected otherwise than by exceeding quota. This can only occur as the final action in a
    * count, hence the name of the method.
    *
    * There are 3 possible outcomes of this method:
    *
    * <ul>
    *   <li>
    *     There is one remaining unfilled vacancy, and two remaining candidates. In this case, the candidate with the
    *     most votes is returned (according to
    *     [[au.id.tmm.countstv.counting.CandidateVoteCountOrdering CandidateVoteCountOrdering]]).
    *   </li>
    *   <li>
    *     The number of unfilled vacancies equals the number of remaining candidates. In this case, the remaining
    *     candidates are returned in order according to
    *     [[au.id.tmm.countstv.counting.CandidateVoteCountOrdering CandidateVoteCountOrdering]].
    *   </li>
    *   <li>
    *     Otherwise, an empty list is returned.
    *   </li>
    * </ul>
    */
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

  def newCandidateStatusesAfterElectionOf[C](
                                              newlyElectedCandidates: DupelessSeq[C],
                                              count: Count,
                                              oldCandidateStatuses: CandidateStatuses[C],
                                            ): CandidateStatuses[C] = {
    val numCandidatesPreviouslyElected = oldCandidateStatuses.electedCandidates.size

    val statusesForNewlyElectedCandidates = newlyElectedCandidates
      .zipWithIndex
      .map { case (newlyElectedCandidate, indexElectedThisStep) =>
        newlyElectedCandidate -> (numCandidatesPreviouslyElected + indexElectedThisStep)
      }
      .map { case (newlyElectedCandidate, ordinalElected) =>
        newlyElectedCandidate -> CandidateStatus.Elected(Ordinal(ordinalElected), count)
      }
      .toMap

    oldCandidateStatuses.updateFrom(statusesForNewlyElectedCandidates)
  }
}
