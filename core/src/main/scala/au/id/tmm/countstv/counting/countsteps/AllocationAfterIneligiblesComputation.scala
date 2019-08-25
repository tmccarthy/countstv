package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.NextActionComputation.NewStatusesAndNextAction
import au.id.tmm.countstv.counting.votecounting.{FullCountVoteCounting, SimpleVoteCounting}
import au.id.tmm.countstv.counting.{NextActionComputation, PaperBundle}
import au.id.tmm.countstv.model.countsteps.AllocationAfterIneligibles
import au.id.tmm.countstv.model.values.Count
import au.id.tmm.probabilitymeasure.ProbabilityMeasure

object AllocationAfterIneligiblesComputation {

  def distributeAwayFromIneligibles[C](
                                        oldContext: CountContext.Initial[C],
                                      ): ProbabilityMeasure[CountContext.AfterIneligibleHandling[C]] = {
    val count = Count.ofIneligibleCandidateHandling
    val oldCandidateStatuses = oldContext.mostRecentCountStep.candidateStatuses

    val newPaperBundlesPerIneligibleCandidate = oldContext.paperBundles
      .filter(b => b.assignedCandidate.exists(oldCandidateStatuses.ineligibleCandidates.contains))
      .groupBy(b => b.assignedCandidate.get)
      .map { case (ineligibleCandidate, paperBundlesForIneligibleCandidate) =>
        val distributionOrigin = PaperBundle.Origin.IneligibleCandidate(ineligibleCandidate)

        val paperBundlesAfterDistribution = paperBundlesForIneligibleCandidate.flatMap { paperBundle =>
          paperBundle.distributeToRemainingCandidates(distributionOrigin, count, oldCandidateStatuses)
        }

        ineligibleCandidate -> paperBundlesAfterDistribution
      }

    val transfersDueToIneligibles = newPaperBundlesPerIneligibleCandidate
      .map { case (candidate, paperBundles) =>
        candidate -> SimpleVoteCounting.performSimpleCount(oldCandidateStatuses.allCandidates, paperBundles)
      }
      .seq

    val replacedPaperBundles = oldContext.paperBundles
      .filterNot(_.assignedCandidate.exists(oldCandidateStatuses.ineligibleCandidates.contains))

    val newPaperBundles = replacedPaperBundles ++
      newPaperBundlesPerIneligibleCandidate.values.flatten

    val newVoteCount = FullCountVoteCounting.performFullRecount(
      oldContext.numFormalPapers,
      oldContext.quota,
      oldCandidateStatuses,
      newPaperBundles,
    )

    val proposedNewCountStep = AllocationAfterIneligibles(
      oldCandidateStatuses,
      newVoteCount,
      transfersDueToIneligibles,
    )

    val proposedCountSteps = oldContext.previousCountSteps.append(proposedNewCountStep)

    NextActionComputation.computeNextAction(oldContext.numVacancies, oldContext.quota, proposedCountSteps)
      .map { case NewStatusesAndNextAction(newCandidateStatuses, nextAction) =>
        val finalisedNewCountStep = proposedNewCountStep.copy(candidateStatuses = newCandidateStatuses)

        oldContext.updated(
          newPaperBundles,
          finalisedNewCountStep,
          nextAction,
        )
      }
  }
}
