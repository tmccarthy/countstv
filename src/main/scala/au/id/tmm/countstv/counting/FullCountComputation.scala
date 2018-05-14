package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.countsteps.distribution.DistributingPapers
import au.id.tmm.countstv.counting.countsteps.{CountContext, FinalElectionComputation, IneligibleHandling, InitialAllocationComputation}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps._
import au.id.tmm.utilities.logging.{LoggedEvent, Logger}
import au.id.tmm.utilities.probabilities.ProbabilityMeasure

import scala.annotation.tailrec

object FullCountComputation {

  implicit val logger: Logger = Logger()

  /**
    * Runs a full count according to the given parameters, returning the count steps through the count.
    */
  def runCount[C](
                   candidates: Set[C],
                   ineligibleCandidates: Set[C],
                   numVacancies: Int,
                   preferenceTree: PreferenceTree[C],
                 ): ProbabilityMeasure[CountSteps[C]] = {

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
      IneligibleHandling.computeContextAfterIneligibles(initialContext.onlyOutcome)
    }




    contextAfterIneligibles.flatMap(allCountStepsFrom)
  }

  @tailrec
  private def allCountStepsFrom[C](previousContext: CountContext[C, CountSteps.AllowingAppending[C]]): ProbabilityMeasure[CountSteps[C]] = {
    if (previousContext.allVacanciesNowFilled) {
      return ProbabilityMeasure.Always(previousContext.previousCountSteps)
    }

    FinalElectionComputation.contextAfterFinalElection(previousContext)
      .map(computeContextAndLog(_)) match {
      case Some(finalCountStepPossibilities) => finalCountStepPossibilities.map(_.previousCountSteps)
      case None => {
        val nextContextPossibilities = computeContextAndLog(DistributingPapers.contextAfterNextCandidateDistribution(previousContext))

        nextContextPossibilities match {
          case ProbabilityMeasure.Always(nextContext) => allCountStepsFrom[C](nextContext)
          case nextContextPossibilities: ProbabilityMeasure.Varied[CountContext[C, CountSteps.DuringDistributions[C]]] => {
            nextContextPossibilities.flatMap(nonRecursiveAllCountStepsFrom[C](_))
          }
        }
      }
    }
  }

  private def nonRecursiveAllCountStepsFrom[C](previousContext: CountContext[C, CountSteps.AllowingAppending[C]]): ProbabilityMeasure[CountSteps[C]] = {
    allCountStepsFrom(previousContext)
  }

  private def computeContextAndLog[C, T_COUNT_STEPS <: CountSteps[C]](
                                                                       countContext: => ProbabilityMeasure[CountContext[C, T_COUNT_STEPS]],
                                                                     ): ProbabilityMeasure[CountContext[C, T_COUNT_STEPS]] = {
    val loggedEvent = LoggedEvent("COUNT_STEP_COMPUTATION")

    val contextToReturn = loggedEvent.logWithTimeOnceFinished {
      val contextToReturn = countContext

      augmentLoggedEventWith(loggedEvent, contextToReturn.anyOutcome) // TODO log all possibilities

      contextToReturn
    }

    contextToReturn
  }

  private def augmentLoggedEventWith[C](loggedEvent: LoggedEvent, countContext: CountContext[C, _]): Unit = {
    val stepType = countContext.mostRecentCountStep match {
      case _: InitialAllocation[C] => "initial"
      case _: AllocationAfterIneligibles[C] => "after_ineligibles"
      case _: DistributionCountStep[C] => "distribution"
      case _: FinalElectionCountStep[C] => "final_election"
    }

    loggedEvent.kvPairs += "count" -> countContext.mostRecentCountStep.count
    loggedEvent.kvPairs += "step_type" -> stepType
    loggedEvent.kvPairs += "num_paper_bundles" -> countContext.paperBundles.size

    countContext.mostRecentCountStep match {
      case DistributionCountStep(_, _, _, source) => {
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
