package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.CountAction._
import au.id.tmm.countstv.counting.countsteps.{AllocationAfterIneligiblesComputation, CountContext, DistributionComputation, FinalElectionComputation}
import au.id.tmm.utilities.probabilities.ProbabilityMeasure

object CountActionInterpreter {

  def applyActionToContext[C](
                               countContext: CountContext.Initial[C],
                             ): ProbabilityMeasure[CountContext.AfterIneligibleHandling[C]] = {
    assert(countContext.nextAction == CountAction.AllocateAwayFromIneligibles)

    AllocationAfterIneligiblesComputation.distributeAwayFromIneligibles(countContext)
  }

  def applyActionToContext[C](
                               countContext: CountContext.AllowingAppending[C],
                             ): ProbabilityMeasure[CountContext.DistributionPhase[C]] = {
    countContext.nextAction match {
      case DistributeFromCandidate(candidate, reason) =>
        DistributionComputation.distributeAwayFromCandidate(countContext, candidate, reason)

      case ElectAllRemainingCandidates =>
        FinalElectionComputation.contextAfterElectingAllRemainingCandidates(countContext)

      case MarkCandidateFinallyElected(candidate) =>
        FinalElectionComputation.contextAfterMarkingCandidateFinallyElected(countContext, candidate)

      case NoAction =>
        throw new NotImplementedError // TODO redesign the types to avoid this being legal
    }
  }

}
