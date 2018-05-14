package au.id.tmm.countstv.counting.countsteps.distribution

import au.id.tmm.countstv.counting.ExcludedCandidateComputations
import au.id.tmm.countstv.counting.countsteps.CountContext
import au.id.tmm.countstv.model.CandidateDistributionReason
import au.id.tmm.countstv.model.values.{NumPapers, TransferValueCoefficient}
import au.id.tmm.utilities.probabilities.ProbabilityMeasure

private[distribution] object NextDistributionComputation {

  final case class DistributionTarget[C](
                                          candidate: C,
                                          reason: CandidateDistributionReason,
                                          transferValueCoefficient: TransferValueCoefficient,
                                        )

  def nextCandidateToDistribute[C](countContext: CountContext[C, _]): ProbabilityMeasure[DistributionTarget[C]] =
    nextElectedDistributionTarget(countContext) getOrElse nextExcludedDistributionTarget(countContext)

  private def nextElectedDistributionTarget[C](countContext: CountContext[C, _]): Option[ProbabilityMeasure[DistributionTarget[C]]] = {
    nextUndistributedElectedCandidate(countContext).map { candidate =>
      val numVotesForCandidate = countContext.mostRecentCountStep.candidateVoteCounts.perCandidate(candidate).numVotes

      DistributionTarget(
        candidate,
        CandidateDistributionReason.Election,
        transferValueCoefficient = TransferValueCoefficient.compute(numVotesForCandidate, countContext.quota),
      )
    }.map(ProbabilityMeasure.always)
  }

  private def nextUndistributedElectedCandidate[C](countContext: CountContext[C, _]): Option[C] = {
    val mostRecentCountStep = countContext.mostRecentCountStep

    mostRecentCountStep
      .candidateStatuses
      .electedCandidates
      .find(c => mostRecentCountStep.candidateVoteCounts.perCandidate(c).numPapers > NumPapers(0))
  }

  private def nextExcludedDistributionTarget[C](countContext: CountContext[C, _]): ProbabilityMeasure[DistributionTarget[C]] = {
    nextCandidateToExclude(countContext).map { candidate =>
      DistributionTarget(
        candidate,
        CandidateDistributionReason.Exclusion,
        TransferValueCoefficient(1d),
      )
    }
  }

  private def nextCandidateToExclude[C](countContext: CountContext[C, _]): ProbabilityMeasure[C] = {
    ExcludedCandidateComputations.computeExcluded(
      countContext.mostRecentCountStep.candidateVoteCounts,
      countContext.previousCandidateVoteCounts.init, // init because we don't want the last one, which we pass above
      countContext.mostRecentCountStep.candidateStatuses,
    )
  }

}
