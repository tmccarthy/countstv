package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.CountAction._
import au.id.tmm.countstv.counting.countsteps.{AllocationAfterIneligiblesComputation, CountContext, DistributionComputation}
import au.id.tmm.countstv.rules.RoundingRules
import au.id.tmm.probabilitymeasure.ProbabilityMeasure

object CountActionInterpreter {

  def applyActionToContext[C](
                               countContext: CountContext.Initial[C],
                             ): ProbabilityMeasure[CountContext.AfterIneligibleHandling[C]] = {
    assert(countContext.nextAction == CountAction.AllocateAwayFromIneligibles)

    AllocationAfterIneligiblesComputation.distributeAwayFromIneligibles(countContext)
  }

  def applyActionToContext[C](
                               countContext: CountContext.AllowingAppending[C],
                             )(implicit
                               roundingRules: RoundingRules,
                             ): ProbabilityMeasure[CountContext.DistributionPhase[C]] = {
    countContext.nextAction match {
      case DistributeFromCandidate(candidate, reason) =>
        DistributionComputation.distributeAwayFromCandidate(countContext, candidate, reason)

      case NoAction =>
        throw new NotImplementedError // TODO redesign the types to avoid this being legal
    }
  }

}
