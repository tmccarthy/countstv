package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.countsteps.{DistributiveCountStepComputation, IneligibleHandling, InitialAllocationComputation}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps._
import au.id.tmm.utilities.logging.{LoggedEvent, Logger}

object FullCountComputation {

  implicit val logger: Logger = Logger()

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

    val initialContext = computeContextAndLog {
      ProbabilityMeasure.always {
        InitialAllocationComputation.computeInitialContext(
          initialCandidateStatuses,
          rootPaperBundle,
          numVacancies,
        )
      }
    }

    val contextAfterIneligibles = computeContextAndLog {
      IneligibleHandling.computeContextAfterIneligibles(initialContext.anyOutcome)
    }

    contextAfterIneligibles.flatMap(allCountStepsFrom)
  }

  private def allCountStepsFrom[C](originalContext: CountContext[C]): ProbabilityMeasure[List[CountStep[C]]] = {

    var currentContext = originalContext

    while (!currentContext.allVacanciesNowFilled) {

      val newContext = computeContextAndLog {
        DistributiveCountStepComputation.computeNextContext(currentContext)
      }

      if (newContext.hasOnlyOneOutcome) {
        currentContext = newContext.onlyOutcome
      } else {
        return newContext.flatMap(allCountStepsFrom)
      }

    }

    logger.info(eventId = "ALL_VACANCIES_FILLED", "count" -> originalContext.mostRecentCountStep.count.countNumber)

    ProbabilityMeasure.always(currentContext.previousCountSteps)
  }

  private def computeContextAndLog[C](
                                       countContext: => ProbabilityMeasure[CountContext[C]],
                                     ): ProbabilityMeasure[CountContext[C]] = {
    val loggedEvent = LoggedEvent("COUNT_STEP_COMPUTATION")

    val contextToReturn = loggedEvent.logWithTimeOnceFinished {
      val contextToReturn = countContext

      augmentLoggedEventWith(loggedEvent, contextToReturn.anyOutcome) // TODO log all possibilities

      contextToReturn
    }

    contextToReturn
  }

  private def augmentLoggedEventWith[C](loggedEvent: LoggedEvent, countContext: CountContext[C]): Unit = {
    val stepType = countContext.mostRecentCountStep match {
      case _: InitialAllocation[C @unchecked] => "initial"
      case _: AllocationAfterIneligibles[C @unchecked] => "after_ineligibles"
      case _: DistributionCountStep[C @unchecked] => "distribution"
    }

    loggedEvent.kvPairs += "count" -> countContext.mostRecentCountStep.count
    loggedEvent.kvPairs += "step_type" -> stepType
    loggedEvent.kvPairs += "num_paper_bundles" -> countContext.paperBundles.size

    countContext.mostRecentCountStep match {
      case DistributionCountStep(_, _, _, Some(source)) => {
        loggedEvent.kvPairs ++= List(
          "distribution_candidate" -> source.candidate,
          "distribution_reason" -> source.candidateDistributionReason,
          "distribution_transfer_value" -> source.transferValue.factor,
        )
      }
      case _ =>
    }

  }

}
