package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.NextActionComputation.NewStatusesAndNextAction
import au.id.tmm.countstv.counting.fixtures.CountContextFixtures
import au.id.tmm.countstv.model.CandidateDistributionReason._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.CandidateStatuses
import au.id.tmm.countstv.model.countsteps.{CountSteps, DistributionCountStep, ElectedNoSurplusCountStep, ExcludedNoVotesCountStep}
import au.id.tmm.countstv.model.values.{Count, Ordinal}
import au.id.tmm.utilities.probabilities.ProbabilityMeasure
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class NextActionComputationSpec extends ImprovedFlatSpec {

  /**
    * Returns the countSteps in the given context, but with the candidate statuses not updated in the last count step.
    * This is to simulate "proposed" countsteps, as required for the computation of the next action and status changes.
    */
  private def countStepsWithoutUpdatedCandidateStatuses[T_COUNT_STEPS <: CountSteps[Fruit]](
                                                                                             countSteps: T_COUNT_STEPS,
                                                                                           ): T_COUNT_STEPS = {
    val secondLastCountStep = countSteps.init.last

    val unUpdatedCandidateStatuses = secondLastCountStep.candidateStatuses

    (
      countSteps match {
        case steps @ CountSteps.AfterIneligibleHandling(_, allocationAfterIneligibles) =>
          steps.copy(allocationAfterIneligibles = allocationAfterIneligibles.copy(candidateStatuses = unUpdatedCandidateStatuses))

        case steps @ CountSteps.DuringDistributions(_, _, distributionCountSteps) => {
          val oldLastCountStep = distributionCountSteps.last

          val newLastCountStep = oldLastCountStep match {
            case c: DistributionCountStep[Fruit] => c.copy(candidateStatuses = unUpdatedCandidateStatuses)
            case c: ExcludedNoVotesCountStep[Fruit] => c.copy(candidateStatuses = unUpdatedCandidateStatuses)
            case c: ElectedNoSurplusCountStep[Fruit] => c.copy(candidateStatuses = unUpdatedCandidateStatuses)
          }

          steps.copy(distributionCountSteps = distributionCountSteps.init :+ newLastCountStep)
        }

        case _: CountSteps.Initial[Fruit] | _: CountSteps.AfterFinalElections[Fruit] => fail()
      }
      ).asInstanceOf[T_COUNT_STEPS]
  }

  "the next action after initial allocation" must "be to allocate away from ineligible candidates" in {
    val context = CountContextFixtures.InitialAllocations.withoutIneligibleCandidates

    val expectedNextActionAndStatuses = NewStatusesAndNextAction(
      context.mostRecentCountStep.candidateStatuses,
      CountAction.AllocateAwayFromIneligibles,
    )

    val actualNextActionAndStatuses = NextActionComputation.computeNextAction(context.previousCountSteps)

    assert(actualNextActionAndStatuses === ProbabilityMeasure.Always(expectedNextActionAndStatuses))
  }

  "the next action after an allocation away from ineligibles in which no candidate exceeds quota" should
    "be to exclude the candidate with the least votes" in {
    val context = CountContextFixtures.AllocationsAfterIneligibles.whereCandidateExcluded

    val expectedNextActionAndStatuses = NewStatusesAndNextAction(
      CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Remaining,
        Watermelon -> Excluded(Ordinal.first, Count(1)),
      ),
      CountAction.DistributeFromCandidate(Watermelon, Exclusion),
    )

    val actualNextActionAndStatuses =
      NextActionComputation.computeNextAction(
        context.numVacancies,
        context.quota,
        countStepsWithoutUpdatedCandidateStatuses(context.previousCountSteps),
      )

    assert(actualNextActionAndStatuses === ProbabilityMeasure.Always(expectedNextActionAndStatuses))
  }

}
