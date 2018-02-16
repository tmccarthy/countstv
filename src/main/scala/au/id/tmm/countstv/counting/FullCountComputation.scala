package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.countsteps.{DistributiveCountStepComputation, IneligibleHandling, InitialAllocationComputation}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountContext, CountStep}

object FullCountComputation {

  def runCount[C](
                   candidates: Set[C],
                   ineligibleCandidates: Set[C],
                   numVacancies: Int,
                   preferenceTree: PreferenceTree[C],
                 ): ProbabilityMeasure[List[CountStep[C]]] = {

    require(ineligibleCandidates.subsetOf(candidates))

    val rootPaperBundle = PaperBundle.rootBundleFor(preferenceTree)

    val initialCandidateStatuses = CandidateStatuses(
      asMap = candidates.map { candidate =>
        val candidateStatus = {
          if (ineligibleCandidates contains candidate) {
            CandidateStatus.Ineligible
          } else {
            CandidateStatus.Remaining
          }
        }

        candidate -> candidateStatus
      }
        .toMap
    )

    val initialContext = InitialAllocationComputation.computeInitialContext(
      initialCandidateStatuses,
      rootPaperBundle,
      numVacancies,
    )

    val contextAfterIneligibles = IneligibleHandling.computeContextAfterIneligibles(initialContext)

    contextAfterIneligibles.flatMap(allCountStepsFrom)
  }

  // TODO should find a way to do this without non-tail recursion
  private def allCountStepsFrom[C](countContext: CountContext[C]): ProbabilityMeasure[List[CountStep[C]]] = {

    if (countContext.allVacanciesNowFilled) {
      ProbabilityMeasure.always(countContext.previousCountSteps)
    } else {
      val newContext = DistributiveCountStepComputation.computeNextContext(countContext)

      newContext.flatMap(allCountStepsFrom)
    }

  }

}
