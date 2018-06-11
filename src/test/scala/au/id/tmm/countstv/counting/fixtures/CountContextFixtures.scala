package au.id.tmm.countstv.counting.fixtures

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.counting.CountAction
import au.id.tmm.countstv.counting.countsteps.{CountContext, FinalElectionComputation}
import au.id.tmm.countstv.model.values.Count

object CountContextFixtures {

  object InitialAllocations {
    def withoutIneligibleCandidates: CountContext.Initial[Fruit] =
      CountFixture.withFinalElection.initialContext

    def withTwoIneligibleCandidates: CountContext.Initial[Fruit] =
      CountFixture.withTwoIneligibleCandidates.initialContext
  }

  object AllocationsAfterIneligibles {
    def whereCandidateExcluded: CountContext.AfterIneligibleHandling[Fruit] =
      CountFixture.withFinalElection.contextAfterIneligibles
  }

  object DuringDistributions {
    def whereExcludedCandidateDistributed: CountContext.DuringDistributions[Fruit] =
      CountFixture.withFinalElection.actualDistributionContextAfterCount(Count(2))

    def whereExcludedCandidateDistributed2: CountContext.DuringDistributions[Fruit] =
      CountFixture.withFinalElection.actualDistributionContextAfterCount(Count(3))

    def whereExcludedCandidateDistributedAndCandidateElected: CountContext.DuringDistributions[Fruit] =
      CountFixture.withFinalElection.actualDistributionContextAfterCount(Count(4))

    def whereElectedCandidatePartiallyDistributed: CountContext.DuringDistributions[Fruit] =
      CountFixture.withFinalElection.actualDistributionContextAfterCount(Count(5))

    def whereCandidateExcludedWithoutVotes: CountContext.DuringDistributions[Fruit] =
      CountFixture.withVotelessCandidate.actualDistributionContextAfterCount(Count(2))

    def whereCandidateElectedWithoutSurplus: CountContext.DuringDistributions[Fruit] =
      CountFixture.withElectionSansSurplus.actualDistributionContextAfterCount(Count(5))
  }

  object AfterFinalStep {
    def whereCandidateElectedToRemainingVacancy: CountContext.Terminal[Fruit] = {
      val contextAfterStep7: CountContext.DuringDistributions[Fruit] =
        CountFixture.withFinalElection.actualDistributionContextAfterCount(Count(7))

      contextAfterStep7.nextAction match {
        case CountAction.MarkCandidateFinallyElected(c) =>
          FinalElectionComputation.contextAfterMarkingCandidateFinallyElected(contextAfterStep7, c).onlyOutcome
            .asInstanceOf[CountContext.Terminal[Fruit]]
        case _ => throw new AssertionError("Expected final election")
      }
    }
  }

}
