package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.countsteps.{CountContext, InitialAllocationComputation}
import au.id.tmm.countstv.model._
import au.id.tmm.utilities.logging.Logger
import au.id.tmm.utilities.probabilities.ProbabilityMeasure
import au.id.tmm.utilities.probabilities.ProbabilityMeasure.{Always, Varied}

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
                 ): ProbabilityMeasure[CompletedCount[C]] = {

    val rootPaperBundle = PaperBundle.rootBundleFor(preferenceTree)

    val initialContextPossibilities = ProbabilityMeasure.Always {
      InitialAllocationComputation.computeInitialContext(
        candidates,
        ineligibleCandidates,
        numVacancies,
        rootPaperBundle,
      )
    }

    val contextAfterIneligiblesPossibilities = initialContextPossibilities.flatMap { initialContext =>
      CountActionInterpreter.applyActionToContext(initialContext)
    }

    val finalContexts = contextAfterIneligiblesPossibilities.flatMap(computeContextUntilFinal)

    finalContexts.map { finalContext =>
      CompletedCount(
        numVacancies,
        finalContext.numFormalPapers,
        finalContext.quota,
        finalContext.previousCountSteps,
      )
    }
  }

  @tailrec
  private def computeContextUntilFinal[C](context: CountContext.AllowingAppending[C]): ProbabilityMeasure[CountContext.DistributionPhase[C]] = {
    val nextContextPossibilities = CountActionInterpreter.applyActionToContext(context)

    nextContextPossibilities match {
      case Always(onlyOutcome) => onlyOutcome match {
        case allowingAppending: CountContext.AllowingAppending[C] =>
          if (allowingAppending.nextAction == CountAction.NoAction) {
            ProbabilityMeasure.Always(allowingAppending)
          } else {
            computeContextUntilFinal(allowingAppending)
          }
        case terminal: CountContext.Terminal[C] => ProbabilityMeasure.Always(terminal)
      }
      case possibilities @ Varied(_) => possibilities.flatMap {
        case allowingAppending: CountContext.AllowingAppending[C] =>
          if (allowingAppending.nextAction == CountAction.NoAction) {
            ProbabilityMeasure.Always(allowingAppending)
          } else {
            nonRecursiveComputeContextUntilFinal(allowingAppending)
          }
        case terminal: CountContext.Terminal[C] => ProbabilityMeasure.Always(terminal)
      }
    }
  }

  private def nonRecursiveComputeContextUntilFinal[C](
                                                       context: CountContext.AllowingAppending[C],
                                                     ): ProbabilityMeasure[CountContext.DistributionPhase[C]] =
    computeContextUntilFinal(context)

}
