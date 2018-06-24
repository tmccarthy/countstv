package au.id.tmm.countstv.counting.fixtures

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.counting.CountAction
import au.id.tmm.countstv.counting.countsteps.{CountContext, FinalElectionComputation}
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
    def withoutIneligibleCandidates: InitialAllocation[Fruit] = assertCountStepType[InitialAllocation[Fruit]] {
      CountFixture.withFinalElection.initialContext.mostRecentCountStep
    }
  }

  object AllocationsAfterIneligibles {
    def whereCandidateExcluded: AllocationAfterIneligibles[Fruit] = assertCountStepType[AllocationAfterIneligibles[Fruit]] {
      CountFixture.withFinalElection.contextAfterIneligibles.mostRecentCountStep
    }
  }

  object DuringDistributions {
    def whereExcludedCandidateDistributed: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(2))
    }

    def whereExcludedCandidateDistributed2: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(3))
    }

    def whereExcludedCandidateDistributedAndCandidateElected: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(4))
    }

    def whereElectedCandidateDistributedAtFractionalTransferValue: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(5))
    }

    def whereExcludedCandidateDistributed3: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(6))
    }

    def whereCandidateExcludedWithoutVotes: ExcludedNoVotesCountStep[Fruit] = assertCountStepType[ExcludedNoVotesCountStep[Fruit]] {
      CountFixture.withVotelessCandidate.getActualCountStep(Count(2))
    }

    def whereCandidateElectedWithoutSurplus: ElectedNoSurplusCountStep[Fruit] = assertCountStepType[ElectedNoSurplusCountStep[Fruit]] {
      CountFixture.withElectionSansSurplus.getActualCountStep(Count(5))
    }

    def wherePapersWorthNoVotesAreDistributed: DistributionCountStep[Fruit] = assertCountStepType[DistributionCountStep[Fruit]] {
      CountFixture.withFinalElection.getActualCountStep(Count(7))
    }
  }

  object AfterFinalStep {
    def whereCandidateElectedToRemainingVacancy: FinalElectionCountStep[Fruit] = assertCountStepType[FinalElectionCountStep[Fruit]] {
      CountContextFixtures.AfterFinalStep.whereCandidateElectedToRemainingVacancy.mostRecentCountStep
    }

    def whereAllRemainingCandidatesMarkedElected: FinalElectionCountStep[Fruit] = assertCountStepType[FinalElectionCountStep[Fruit]] {
      val contextAfterStep1: CountContext.AfterIneligibleHandling[Fruit] =
        CountFixture.withAVacancyForEachCandidate.contextAfterIneligibles

      contextAfterStep1.nextAction match {
        case CountAction.ElectAllRemainingCandidates =>
          FinalElectionComputation.contextAfterElectingAllRemainingCandidates(contextAfterStep1).onlyOutcome
            .asInstanceOf[CountContext.Terminal[Fruit]]
            .mostRecentCountStep
        case _ => throw new AssertionError("Expected final election")
      }
    }
  }

}
