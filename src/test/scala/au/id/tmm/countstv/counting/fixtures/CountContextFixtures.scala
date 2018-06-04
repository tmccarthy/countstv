package au.id.tmm.countstv.counting.fixtures

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.counting.countsteps.{CountContext, FinalElectionComputation}
import au.id.tmm.countstv.model.countsteps.CountSteps
import au.id.tmm.countstv.model.values.Count

object CountContextFixtures {

  object InitialAllocations {
    val withoutIneligibleCandidates: CountContext[Fruit, CountSteps.Initial[Fruit]] =
      CountFixture.withFinalElection.initialContext
  }

  object AllocationsAfterIneligibles {
    val whereCandidateExcluded: CountContext[Fruit, CountSteps.AfterIneligibleHandling[Fruit]] =
      CountFixture.withFinalElection.contextAfterIneligibles
  }

  object DuringDistributions {
    val whereExcludedCandidateDistributed: CountContext[Fruit, CountSteps.DuringDistributions[Fruit]] =
      CountFixture.withFinalElection.actualContextAfterCount(Count(2))

    val whereExcludedCandidateDistributed2: CountContext[Fruit, CountSteps.DuringDistributions[Fruit]] =
      CountFixture.withFinalElection.actualContextAfterCount(Count(3))

    val whereExcludedCandidateDistributedAndCandidateElected: CountContext[Fruit, CountSteps.DuringDistributions[Fruit]] =
      CountFixture.withFinalElection.actualContextAfterCount(Count(4))

    val whereElectedCandidatePartiallyDistributed: CountContext[Fruit, CountSteps.DuringDistributions[Fruit]] =
      CountFixture.withFinalElection.actualContextAfterCount(Count(5))

    val whereCandidateExcludedWithoutVotes: CountContext[Fruit, CountSteps.DuringDistributions[Fruit]] =
      CountFixture.withVotelessCandidate.actualContextAfterCount(Count(2))

    val whereCandidateElectedWithoutSurplus: CountContext[Fruit, CountSteps.DuringDistributions[Fruit]] =
      CountFixture.withElectionSansSurplus.actualContextAfterCount(Count(5))
  }

  object AfterFinalStep {
    val whereCandidateElectedToRemainingVacancy: CountContext[Fruit, CountSteps.AfterFinalElections[Fruit]] = {
      val contextAfterStep7 = CountFixture.withFinalElection.actualContextAfterCount(Count(7))

      val actualContext = FinalElectionComputation.contextAfterFinalElection(contextAfterStep7)

      actualContext.getOrElse(throw new AssertionError("Expected final election")).onlyOutcome
    }
  }

}
