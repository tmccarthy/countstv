package au.id.tmm.countstv.counting.fixtures

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.counting.countsteps.FinalElectionComputation
import au.id.tmm.countstv.model.countsteps._
import au.id.tmm.countstv.model.values.Count
import org.scalatest.Assertions

import scala.reflect.ClassTag

object CountStepFixtures {

  // TODO ideally we would structure the types so this isn't needed
  private def assertCountStepType[A : ClassTag](countStep: => CountStep[Fruit]): A = {
    countStep match {
      case c: A => c
      case _ => Assertions.fail("Count step was not the expected type")
    }
  }

  object InitialAllocations {
    val withoutIneligibleCandidates: InitialAllocation[Fruit] = assertCountStepType[InitialAllocation[Fruit]] {
      CountFixture.withFinalElection.initialContext.mostRecentCountStep
    }
  }

  object AllocationsAfterIneligibles {
    val whereCandidateExcluded: AllocationAfterIneligibles[Fruit] = assertCountStepType[AllocationAfterIneligibles[Fruit]] {
      CountFixture.withFinalElection.contextAfterIneligibles.mostRecentCountStep
    }
  }

  object DuringDistributions {
    val whereExcludedCandidateDistributed: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(2))
    }

    val whereExcludedCandidateDistributed2: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(3))
    }

    val whereExcludedCandidateDistributedAndCandidateElected: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(4))
    }

    val whereElectedCandidatePartiallyDistributed: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(5))
    }

    val whereElectedCandidatePartiallyDistributedAtFractionalTransferValue: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(6))
    }

    val whereCandidateExcludedWithoutVotes: ExcludedNoVotesCountStep[Fruit] = assertCountStepType[ExcludedNoVotesCountStep[Fruit]] {
      CountFixture.withVotelessCandidate.getActualCountStep(Count(2))
    }

    val whereCandidateElectedWithoutSurplus: ElectedNoSurplusCountStep[Fruit] = assertCountStepType[ElectedNoSurplusCountStep[Fruit]] {
      CountFixture.withElectionSansSurplus.getActualCountStep(Count(5))
    }
  }

  object AfterFinalStep {
    val whereCandidateElectedToRemainingVacancy: FinalElectionCountStep[Fruit] = assertCountStepType[FinalElectionCountStep[Fruit]] {
      val contextAfterStep7 = CountFixture.withFinalElection.actualContextAfterCount(Count(7))

      val actualContext = FinalElectionComputation.contextAfterFinalElection(contextAfterStep7)

      actualContext.getOrElse(throw new AssertionError("Expected final election")).onlyOutcome.mostRecentCountStep
    }
  }

}
