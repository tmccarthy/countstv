package au.id.tmm.countstv.counting.countsteps.distribution

import au.id.tmm.countstv.counting._
import au.id.tmm.countstv.counting.countsteps.CountContext
import au.id.tmm.countstv.counting.countsteps.distribution.NextDistributionComputation.DistributionTarget
import au.id.tmm.countstv.model.CandidateDistributionReason.Exclusion
import au.id.tmm.countstv.model.countsteps.{CountSteps, DistributionCountStep, ExcludedNoVotesCountStep}
import au.id.tmm.countstv.model.values.{Count, NumVotes, Ordinal, TransferValue}
import au.id.tmm.countstv.model.{CandidateDistributionReason, CandidateStatus, CandidateStatuses, CandidateVoteCounts}
import au.id.tmm.utilities.probabilities.ProbabilityMeasure

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.parallel.immutable.ParSet

private[counting] object DistributingPapers {

  def contextAfterNextCandidateDistribution[C](
                                                countContext: CountContext[C, _ <: CountSteps.AllowingAppending[C]],
                                              ): ProbabilityMeasure[CountContext[C, CountSteps.DuringDistributions[C]]] = {
    val distributionTarget = NextDistributionComputation.nextCandidateToDistribute(countContext)

    distributionTarget.flatMap(applyDistributionToContext(countContext, _))
  }

  private def applyDistributionToContext[C](
                                             countContext: CountContext[C, _ <: CountSteps.AllowingAppending[C]],
                                             distributionTarget: DistributionTarget[C],
                                           ): ProbabilityMeasure[CountContext[C, CountSteps.DuringDistributions[C]]] = {
    val bundlesToDistribute = computeBundlesToDistribute(countContext, distributionTarget)

    appendDistributionCountSteps(countContext, distributionTarget, bundlesToDistribute)
  }

  private def computeBundlesToDistribute[C](
                                             countContext: CountContext[C, _ <: CountSteps.AllowingAppending[C]],
                                             distributionTarget: DistributionTarget[C],
                                           ): Queue[ParSet[AssignedPaperBundle[C]]] = {
    // TODO what if there are no bundles?

    val bundlesPerTransferValue =
      new mutable.TreeMap[TransferValue, mutable.Set[AssignedPaperBundle[C]]]()(TransferValue.ordering.reverse)

    for (bundle <- countContext.paperBundles.seq) {
      bundle match {
        case b: AssignedPaperBundle[C] if b.assignedCandidate.contains(distributionTarget.candidate) =>
          bundlesPerTransferValue.getOrElseUpdate(b.transferValue, mutable.Set.empty) += b
        case _ =>
      }
    }

    bundlesPerTransferValue
      .valuesIterator
      .map(_.to[ParSet])
      .to[Queue]
  }

  @tailrec
  private def appendDistributionCountSteps[C](
                                               countContext: CountContext[C, _ <: CountSteps.AllowingAppending[C]],
                                               distributionTarget: DistributionTarget[C],
                                               bundlesToDistribute: Queue[ParSet[AssignedPaperBundle[C]]],
                                             ): ProbabilityMeasure[CountContext[C, CountSteps.DuringDistributions[C]]] = {

    val count = countContext.mostRecentCountStep.count.increment
    val candidateStatusesDuringStep = updateStatusOfAnyNewlyExcluded(
      count,
      countContext.mostRecentCountStep.candidateStatuses,
      distributionTarget,
    )

    if (bundlesToDistribute.isEmpty && distributionTarget.reason == Exclusion) {

      val newCountStep = ExcludedNoVotesCountStep(
          count,
          candidateStatusesDuringStep,
          countContext.mostRecentCountStep.candidateVoteCounts,
          distributionTarget.candidate,
        )

      val newContext = countContext.copy(
        previousCountSteps = countContext.previousCountSteps.append(newCountStep)
      )

      return ProbabilityMeasure.Always(newContext)
    }

    val (bundlesToDistributeNow, bundlesToDistributeLater) = bundlesToDistribute.dequeue

    val paperBundlesAfterDistribution = allPaperBundlesAfterDistributingSome(
      count,
      candidateStatusesDuringStep,
      distributionTarget,
      countContext.paperBundles,
      bundlesToDistributeNow,
    )

    val newVoteCounts = VoteCounting.countVotes(
      countContext.numFormalPapers,
      countContext.quota,
      candidateStatusesDuringStep,
      paperBundlesAfterDistribution,
    )

    val sourceForCountStep = DistributionCountStep.Source(
      distributionTarget.candidate,
      distributionTarget.reason,
      bundlesToDistributeNow.map(_.origin.count).seq,
      distributionTarget.transferValueCoefficient * bundlesToDistributeNow.head.transferValue,
    )

    val newCountContextPossibilities = updateStatusesOfAnyNewlyElected(
      count,
      countContext.quota,
      countContext.numVacancies,
      candidateStatusesDuringStep,
      newVoteCounts,
      countContext.previousCandidateVoteCounts,
    ).map { candidateStatusesAtEndOfStep =>
      val newCountStep = DistributionCountStep(
        count,
        candidateStatusesAtEndOfStep,
        newVoteCounts,
        sourceForCountStep,
      )

      countContext.copy(
        paperBundles = paperBundlesAfterDistribution,
        previousCountSteps = countContext.previousCountSteps.append(newCountStep),
      )
    }

    if (bundlesToDistributeLater.isEmpty) {
      newCountContextPossibilities
    } else {
      newCountContextPossibilities match {
        case ProbabilityMeasure.Always(newCountContext) => {
          appendDistributionCountSteps(newCountContext, distributionTarget, bundlesToDistributeLater)
        }
        case newCountContextPossibilities@ProbabilityMeasure.Varied(_) => {
          newCountContextPossibilities.flatMap { newCountContext =>
            nonRecursiveAppendDistributionCountSteps(newCountContext, distributionTarget, bundlesToDistributeLater)
          }
        }
      }
    }
  }

  private def nonRecursiveAppendDistributionCountSteps[C](
                                                           countContext: CountContext[C, _ <: CountSteps.AllowingAppending[C]],
                                                           distributionTarget: DistributionTarget[C],
                                                           bundlesToDistribute: Queue[ParSet[AssignedPaperBundle[C]]],
                                                         ): ProbabilityMeasure[CountContext[C, CountSteps.DuringDistributions[C]]] = {
    appendDistributionCountSteps(countContext, distributionTarget, bundlesToDistribute)
  }

  private def updateStatusOfAnyNewlyExcluded[C](
                                                 count: Count,
                                                 candidateStatuses: CandidateStatuses[C],
                                                 distributionTarget: DistributionTarget[C],
                                               ): CandidateStatuses[C] = {
    val targetCandidate = distributionTarget.candidate
    val statusOfCandidate = candidateStatuses.asMap(targetCandidate)

    (distributionTarget.reason, statusOfCandidate) match {
      case (_, CandidateStatus.Excluded(_, _)) => candidateStatuses
      case (CandidateDistributionReason.Exclusion, _) => {
        val newStatus = CandidateStatus.Excluded(
          ordinalExcluded = Ordinal.ofNextAdditionTo(candidateStatuses.excludedCandidates),
          excludedAtCount = count,
        )
        candidateStatuses.update(targetCandidate, newStatus)
      }
      case (_, _) => candidateStatuses
    }
  }

  private def allPaperBundlesAfterDistributingSome[C](
                                                       count: Count,
                                                       candidateStatuses: CandidateStatuses[C],
                                                       distributionTarget: DistributionTarget[C],
                                                       allPaperBundles: PaperBundles[C],
                                                       bundlesToDistribute: ParSet[AssignedPaperBundle[C]],
                                                     ): PaperBundles[C] = {
    val distributionOrigin = distributionTarget match {
      case DistributionTarget(candidate, CandidateDistributionReason.Exclusion, _) =>
        PaperBundle.Origin.ExcludedCandidate(candidate, count)
      case DistributionTarget(candidate, CandidateDistributionReason.Election, transferValueCoefficient) =>
        PaperBundle.Origin.ElectedCandidate(candidate, transferValueCoefficient, count)
    }

    val newBundles = bundlesToDistribute.flatMap { bundle =>
      bundle.distributeToRemainingCandidates(
        distributionOrigin,
        count,
        candidateStatuses,
      )
    }

    (allPaperBundles diff bundlesToDistribute.asInstanceOf[ParSet[PaperBundle[C]]]) union newBundles
  }

  private def updateStatusesOfAnyNewlyElected[C](
                                                  count: Count,
                                                  quota: NumVotes,
                                                  numVacancies: Int,
                                                  candidateStatuses: CandidateStatuses[C],
                                                  newVoteCounts: CandidateVoteCounts[C],
                                                  previousCandidateVoteCounts: List[CandidateVoteCounts[C]],
                                                ): ProbabilityMeasure[CandidateStatuses[C]] = {
    ElectedCandidateComputations.newlyExceedingQuota(
      newVoteCounts,
      previousCandidateVoteCounts,
      candidateStatuses,
      numVacancies,
      quota,
    ).map { newlyElectedCandidates =>
      ElectedCandidateComputations.newCandidateStatusesAfterElectionOf(newlyElectedCandidates, count, candidateStatuses)
    }
  }

}