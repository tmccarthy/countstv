package au.id.tmm.countstv.counting

import au.id.tmm.countstv.counting.countsteps.{CountContext, InitialAllocationComputation}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.preferences.PreferenceTree
import au.id.tmm.countstv.rules.RoundingRules
import au.id.tmm.probability.measure.ProbabilityMeasure
import au.id.tmm.probability.measure.ProbabilityMeasure.{Always, Varied}

import scala.annotation.tailrec

object FullCountComputation {

  /**
    * Runs a full count according to the given parameters, returning the count steps through the count.
    */
  def runCount[C](
    params: CountParams[C],
    preferenceTree: PreferenceTree[C],
  ): ProbabilityMeasure[CompletedCount[C]] = {

    val rootPaperBundle = PaperBundle.rootBundleFor(preferenceTree)

    implicit val roundingRules: RoundingRules = params.roundingRules

    val initialContextPossibilities = ProbabilityMeasure.Always {
      InitialAllocationComputation.computeInitialContext(
        params.candidates,
        params.ineligibleCandidates,
        params.numVacancies,
        rootPaperBundle,
      )
    }

    val contextAfterIneligiblesPossibilities = initialContextPossibilities.flatMap { initialContext =>
      CountActionInterpreter.applyActionToContext(initialContext)
    }

    val finalContexts = contextAfterIneligiblesPossibilities.flatMap(computeContextUntilFinal)

    finalContexts.map { finalContext =>
      CompletedCount(
        params,
        finalContext.numFormalPapers,
        finalContext.quota,
        finalContext.previousCountSteps,
      )
    }
  }

  @tailrec
  private def computeContextUntilFinal[C](
    context: CountContext.AllowingAppending[C],
  )(implicit
    roundingRules: RoundingRules,
  ): ProbabilityMeasure[CountContext.AllowingAppending[C]] = {
    if (context.nextAction == CountAction.NoAction) {
      return Always(context)
    }

    val nextContextPossibilities = CountActionInterpreter.applyActionToContext(context)

    nextContextPossibilities match {
      case Always(onlyOutcome) => computeContextUntilFinal(onlyOutcome)
      case possibilities: Varied[CountContext.DistributionPhase[C]] =>
        possibilities.flatMap { possibility =>
          nonRecursiveComputeContextUntilFinal(possibility)
        }
    }
  }

  private def nonRecursiveComputeContextUntilFinal[C](
    context: CountContext.AllowingAppending[C],
  )(implicit
    roundingRules: RoundingRules,
  ): ProbabilityMeasure[CountContext.AllowingAppending[C]] =
    computeContextUntilFinal(context)

}
