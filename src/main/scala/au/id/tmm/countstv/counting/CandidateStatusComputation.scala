package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts}
import au.id.tmm.utilities.collection.OrderedSet

object CandidateStatusComputation {
  def computeElected[C](
                         counts: CandidateVoteCounts[C],
                         candidateStatuses: CandidateStatuses[C],
                         numVacancies: Int,
                         quota: Long,
                       ): OrderedSet[C] = {
    val alreadyElected = candidateStatuses
      .electedCandidates

    val numUnfilledVacancies = numVacancies - alreadyElected.size

    val newlyElected = {
      if (alreadyElected.size >= numVacancies) {
        // All vacancies have been filled
        Set.empty

      } else if (candidateStatuses.remainingCandidates.size <= numUnfilledVacancies) {
        // We've excluded enough candidates that we can just mark all remaining candidates as elected, in order of the
        // number of votes they have
        candidateStatuses.remainingCandidates
          .toStream
          .sortBy(c => counts.perCandidate(c).numVotes)
          .to[OrderedSet]

      } else {
        // We elect all remaining candidates whose vote is above the quota

        candidateStatuses.remainingCandidates
          .toStream
          .filter { candidate => counts.perCandidate(candidate).numVotes > quota }
          .sortBy { candidate => counts.perCandidate(candidate).numVotes }
          .to[OrderedSet]

      }
    }

    alreadyElected ++ newlyElected
  }


}
