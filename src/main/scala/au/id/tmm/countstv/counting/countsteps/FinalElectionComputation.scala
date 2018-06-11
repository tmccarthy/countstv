package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.{CandidateVoteCountOrdering, CountAction, ElectedCandidateComputations}
import au.id.tmm.countstv.model.countsteps.FinalElectionCountStep
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.probabilities.{ProbabilityMeasure, TieSensitiveSorting}

object FinalElectionComputation {

  def contextAfterElectingAllRemainingCandidates[C](
                                                     countContext: CountContext.AllowingAppending[C],
                                                   ): ProbabilityMeasure[CountContext.DistributionPhase[C]] = {
    assert(countContext.nextAction == CountAction.ElectAllRemainingCandidates)

    val candidateStatuses = countContext.mostRecentCountStep.candidateStatuses

    val ordering = candidateOrdering(countContext)

    TieSensitiveSorting.sort(candidateStatuses.remainingCandidates)(ordering)
      .map(_.reverse.to[DupelessSeq])
      .map(contextGivenNewlyElected(countContext, _))
  }

  def contextAfterMarkingCandidateFinallyElected[C](
                                                     countContext: CountContext.AllowingAppending[C],
                                                     candidateFinallyElected: C,
                                                   ): ProbabilityMeasure[CountContext.DistributionPhase[C]] = {
    ProbabilityMeasure.Always(contextGivenNewlyElected(countContext, DupelessSeq(candidateFinallyElected)))
  }

  private def candidateOrdering[C](countContext: CountContext.AllowingAppending[C]): CandidateVoteCountOrdering[C] = {
    val currentCandidateVoteCounts = countContext.mostRecentCountStep.candidateVoteCounts
    val previousCandidateVoteCountsAscending = countContext.previousCountSteps.tail.dropRight(1).map(_.candidateVoteCounts).toList

    new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)
  }

  private def contextGivenNewlyElected[C](
                                           countContext: CountContext.AllowingAppending[C],
                                           newlyElectedCandidates: DupelessSeq[C],
                                         ): CountContext.DistributionPhase[C] = {
    val count = countContext.mostRecentCountStep.count.increment

    val newCandidateStatuses = ElectedCandidateComputations.newCandidateStatusesAfterElectionOf(
      newlyElectedCandidates,
      count,
      countContext.mostRecentCountStep.candidateStatuses,
    )

    val newCountStep = FinalElectionCountStep(count, newCandidateStatuses, countContext.mostRecentCountStep.candidateVoteCounts)

    countContext.updated(
      newCountStep,
      nextAction = CountAction.NoAction,
    )
  }

}
