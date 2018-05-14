package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.ElectedCandidateComputations
import au.id.tmm.countstv.model.countsteps.{CountSteps, FinalElectionCountStep}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.probabilities.ProbabilityMeasure

object FinalElectionComputation {

  def contextAfterFinalElection[C](
                                    countContext: CountContext[C, CountSteps.AllowingAppending[C]],
                                  ): Option[ProbabilityMeasure[CountContext[C, CountSteps.AfterFinalElections[C]]]] = {
    ElectedCandidateComputations.finallyElected(
      countContext.mostRecentCountStep.candidateVoteCounts,
      countContext.previousCandidateVoteCounts.init,
      countContext.candidateStatuses,
      countContext.numVacancies,
      countContext.quota,
    ) match {
      case ProbabilityMeasure.Always(DupelessSeq()) => None
      case p => Some(
        p.map { newlyElectedCandidates =>
          val count = countContext.mostRecentCountStep.count.increment

          val newCandidateStatuses = ElectedCandidateComputations.newCandidateStatusesAfterElectionOf(newlyElectedCandidates, count, countContext.candidateStatuses)

          val newCountStep = FinalElectionCountStep(count, newCandidateStatuses, countContext.mostRecentCountStep.candidateVoteCounts)

          countContext.copy(
            candidateStatuses = newCandidateStatuses,
            previousCountSteps = countContext.previousCountSteps.append(newCountStep)
          )
        }
      )
    }
  }

}
