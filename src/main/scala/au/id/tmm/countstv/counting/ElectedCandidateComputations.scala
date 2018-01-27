package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, ProbabilityMeasure}
import au.id.tmm.utilities.collection.DupelessSeq

object ElectedCandidateComputations {

  def computeElected[C](
                         counts: CandidateVoteCounts[C],
                         candidateStatuses: CandidateStatuses[C],
                         numVacancies: Int,
                         quota: Long,
                       ): ProbabilityMeasure[DupelessSeq[C]] = {
    val alreadyElected = candidateStatuses
      .electedCandidates

    val numUnfilledVacancies = numVacancies - alreadyElected.size

    val newlyElected: ProbabilityMeasure[DupelessSeq[C]] = {
      if (alreadyElected.size >= numVacancies) {
        // All vacancies have been filled
        ProbabilityMeasure.always(DupelessSeq.empty)

      } else if (candidateStatuses.remainingCandidates.size <= numUnfilledVacancies) {
        // We've excluded enough candidates that we can just mark all remaining candidates as elected, in order of the
        // number of votes they have
        TieSensitiveSorting
          .sortBy(candidateStatuses.remainingCandidates)(c => counts.perCandidate(c).numVotes)
          .map(_.reverse)

      } else {
        // We elect all remaining candidates whose vote is above the quota

        val remainingCandidatesAboveQuota = candidateStatuses.remainingCandidates
          .toStream
          .filter { candidate => counts.perCandidate(candidate).numVotes > quota }

        TieSensitiveSorting.sortBy(remainingCandidatesAboveQuota)(candidate => counts.perCandidate(candidate).numVotes)
          .map(_.take(numUnfilledVacancies))
          .map(_.reverse)
      }
    }

    newlyElected
      .map(alreadyElected ++ _)
  }

}
