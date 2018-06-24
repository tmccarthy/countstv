package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.NextActionComputation.NewStatusesAndNextAction
import au.id.tmm.countstv.counting._
import au.id.tmm.countstv.counting.votecounting.FullCountVoteCounting
import au.id.tmm.countstv.model.CandidateDistributionReason._
import au.id.tmm.countstv.model.countsteps.{DistributionCountStep, ElectedNoSurplusCountStep, ExcludedNoVotesCountStep}
import au.id.tmm.countstv.model.values._
import au.id.tmm.countstv.model.{CandidateDistributionReason, CandidateStatuses, CandidateVoteCounts}
import au.id.tmm.utilities.probabilities.ProbabilityMeasure
import au.id.tmm.utilities.probabilities.ProbabilityMeasure.{Always, Varied}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.parallel.immutable.ParSet

object DistributionComputation {

  def distributeAwayFromCandidate[C](
                                      countContext: CountContext.AllowingAppending[C],
                                      candidate: C,
                                      distributionReason: CandidateDistributionReason,
                                    ): ProbabilityMeasure[CountContext.DistributionPhase[C]] = {
    val transferValueCoefficient = computeTransferValueCoefficient(countContext, candidate, distributionReason)

    val bundlesToDistribute = computeBundlesToDistribute(countContext, candidate, distributionReason)

    if (distributionReason == Election && transferValueCoefficient == TransferValueCoefficient(0)) {
      applyElectionWithoutSurplus(countContext, candidate)

    } else if (distributionReason == Exclusion && bundlesToDistribute.isEmpty) {
      applyExclusionWithoutPapers(countContext, candidate)

    } else {
      applyDistributionsUntilAllBundlesDistributed(
        countContext,
        candidate,
        distributionReason,
        transferValueCoefficient,
        bundlesToDistribute,
      )
    }
  }

  private def computeTransferValueCoefficient[C](
                                                  countContext: CountContext.AllowingAppending[C],
                                                  candidate: C,
                                                  distributionReason: CandidateDistributionReason,
                                                ) = {
    if (distributionReason == Election) {
      val voteCountForCandidate = countContext.mostRecentCountStep.candidateVoteCounts.perCandidate(candidate)

      TransferValueCoefficient.compute(voteCountForCandidate.numVotes, countContext.quota)
    } else {
      TransferValueCoefficient(1d)
    }
  }

  private def computeBundlesToDistribute[C](
                                             countContext: CountContext.AllowingAppending[C],
                                             candidateToDistribute: C,
                                             distributionReason: CandidateDistributionReason,
                                           ): Queue[ParSet[AssignedPaperBundle[C]]] = {
    distributionReason match {
      case Election => Queue(
        countContext.paperBundles.collect {
          case b: AssignedPaperBundle[C] if b.assignedCandidate.contains(candidateToDistribute) => b
        }
      )
      case Exclusion => {
        val bundlesPerTransferValue =
          new mutable.TreeMap[TransferValue, mutable.Set[AssignedPaperBundle[C]]]()(TransferValue.ordering.reverse)

        for (bundle <- countContext.paperBundles.seq) {
          bundle match {
            case b: AssignedPaperBundle[C] if b.assignedCandidate.contains(candidateToDistribute) =>
              bundlesPerTransferValue.getOrElseUpdate(b.transferValue, mutable.Set.empty) += b
            case _ =>
          }
        }

        bundlesPerTransferValue
          .valuesIterator
          .map(_.to[ParSet])
          .to[Queue]
      }
    }
  }

  private def applyElectionWithoutSurplus[C](
                                              countContext: CountContext.AllowingAppending[C],
                                              candidate: C,
                                            ): ProbabilityMeasure[CountContext.DistributionPhase[C]] = {
    val count = countContext.mostRecentCountStep.count.increment

    val paperBundlesForCandidate: PaperBundles[C] = countContext.paperBundles.filter(_.assignedCandidate contains candidate)

    val newPaperBundles = countContext.paperBundles diff paperBundlesForCandidate

    val oldCandidateStatuses = countContext.mostRecentCountStep.candidateStatuses

    val newVoteCounts = FullCountVoteCounting.performFullRecount(
      countContext.numFormalPapers,
      countContext.quota,
      candidateStatuses = oldCandidateStatuses,
      newPaperBundles,
    )

    val proposedCountStep = ElectedNoSurplusCountStep(
      count,
      oldCandidateStatuses,
      newVoteCounts,
      candidate,
      sourceCounts = paperBundlesForCandidate.map(_.origin.count).seq,
    )

    val proposedCountSteps = countContext.previousCountSteps.append(proposedCountStep)

    NextActionComputation.computeNextAction(countContext.numVacancies, countContext.quota, proposedCountSteps)
      .map { case NewStatusesAndNextAction(newStatuses, nextAction) =>
        val newCountStep = proposedCountStep.copy(candidateStatuses = newStatuses)

        countContext.updated(
          newPaperBundles,
          newCountStep,
          nextAction,
        )
      }
  }

  private def applyExclusionWithoutPapers[C](
                                              countContext: CountContext.AllowingAppending[C],
                                              candidate: C,
                                            ): ProbabilityMeasure[CountContext.DistributionPhase[C]] = {
    val count = countContext.mostRecentCountStep.count.increment

    val oldCandidateStatuses = countContext.mostRecentCountStep.candidateStatuses

    val proposedCountStep = ExcludedNoVotesCountStep(
      count,
      oldCandidateStatuses,
      countContext.mostRecentCountStep.candidateVoteCounts,
      candidate,
    )

    val proposedCountSteps = countContext.previousCountSteps.append(proposedCountStep)

    NextActionComputation.computeNextAction(countContext.numVacancies, countContext.quota, proposedCountSteps)
      .map { case NewStatusesAndNextAction(newStatuses, nextAction) =>
        val newCountStep = proposedCountStep.copy(candidateStatuses = newStatuses)

        countContext.updated(
          countContext.paperBundles,
          newCountStep,
          nextAction,
        )
      }
  }

  private def nonRecursiveApplyDistributionsUntilAllBundlesDistributed[C](
                                                                           countContext: CountContext.AllowingAppending[C],
                                                                           candidateToDistribute: C,
                                                                           distributionReason: CandidateDistributionReason,

                                                                           transferValueCoefficient: TransferValueCoefficient,
                                                                           bundlesToDistribute: Queue[ParSet[AssignedPaperBundle[C]]],
                                                                         ): ProbabilityMeasure[CountContext.DistributionPhase[C]] =
    applyDistributionsUntilAllBundlesDistributed(
      countContext,
      candidateToDistribute,
      distributionReason,
      transferValueCoefficient,
      bundlesToDistribute,
    )

  @tailrec
  private def applyDistributionsUntilAllBundlesDistributed[C](
                                                               countContext: CountContext.AllowingAppending[C],
                                                               candidateToDistribute: C,
                                                               distributionReason: CandidateDistributionReason,

                                                               transferValueCoefficient: TransferValueCoefficient,
                                                               bundlesToDistribute: Queue[ParSet[AssignedPaperBundle[C]]],
                                                             ): ProbabilityMeasure[CountContext.DistributionPhase[C]] = {
    val count = countContext.mostRecentCountStep.count.increment

    val oldCandidateStatuses = countContext.mostRecentCountStep.candidateStatuses

    val (bundlesToDistributeNow, bundlesToDistributeLater) = bundlesToDistribute.dequeue

    val BundleUpdate(newBundles, paperBundlesAfterDistribution) = allPaperBundlesAfterDistributingSome(
      count,
      oldCandidateStatuses,
      candidateToDistribute,
      distributionReason,
      transferValueCoefficient,
      countContext.paperBundles,
      bundlesToDistributeNow,
    )

    val newVoteCounts = FullCountVoteCounting.performFullRecount(
      countContext.numFormalPapers,
      countContext.quota,
      oldCandidateStatuses,
      paperBundlesAfterDistribution,
    )

    val distributionSource = DistributionCountStep.Source(
      candidateToDistribute,
      distributionReason,
      bundlesToDistributeNow.map(_.origin.count).seq,
      transferValue = if (distributionReason == Exclusion) {
        // All the transfer values are the same so we don't have to compute the weighted value
        transferValueCoefficient * bundlesToDistributeNow.head.transferValue
      } else {
        transferValueCoefficient * transferValueOf(bundlesToDistributeNow)
      },
    )

    if (bundlesToDistributeLater.nonEmpty) {

      // Because we know what the the next count step will be (the continued distribution of these bundles) we can
      // handle candidate status changes ourselves.

      val newCountContextPossibilities = updateStatusesOfAnyNewlyElected(
        count,
        countContext.quota,
        countContext.numVacancies,
        oldCandidateStatuses,
        newVoteCounts,
        previousCandidateVoteCounts = countContext.previousCountSteps.tail.map(_.candidateVoteCounts).toList,
      ).map { newCandidateStatuses =>
        val newCountStep = DistributionCountStep(
          count,
          newCandidateStatuses,
          newVoteCounts,
          distributionSource,
        )

        countContext.updated(
          paperBundlesAfterDistribution,
          newCountStep,
          nextAction = countContext.nextAction,
        )
      }

      newCountContextPossibilities match {
        case Always(newCountContext) =>
          applyDistributionsUntilAllBundlesDistributed(
            newCountContext,
            candidateToDistribute,
            distributionReason,
            transferValueCoefficient,
            bundlesToDistributeLater,
          )
        case possibilities @ Varied(_) => possibilities.flatMap { newCountContextPossibility =>
          nonRecursiveApplyDistributionsUntilAllBundlesDistributed(
            newCountContextPossibility,
            candidateToDistribute,
            distributionReason,
            transferValueCoefficient,
            bundlesToDistributeLater,
          )
        }
      }
    } else {
      val proposedCountStep = DistributionCountStep(
        count,
        oldCandidateStatuses,
        newVoteCounts,
        distributionSource,
      )

      val proposedCountSteps = countContext.previousCountSteps.append(proposedCountStep)

      NextActionComputation.computeNextAction(countContext.numVacancies, countContext.quota, proposedCountSteps)
        .map { case NewStatusesAndNextAction(newStatuses, nextAction) =>
          val newCountStep = proposedCountStep.copy(candidateStatuses = newStatuses)

          countContext.updated(
            paperBundlesAfterDistribution,
            newCountStep,
            nextAction,
          )
        }
    }
  }

  private final case class BundleUpdate[C](newBundles: PaperBundles[C], allBundlesIncludingNewOnes: PaperBundles[C])

  private def allPaperBundlesAfterDistributingSome[C](
                                                       count: Count,
                                                       candidateStatuses: CandidateStatuses[C],

                                                       candidateToDistribute: C,
                                                       distributionReason: CandidateDistributionReason,
                                                       transferValueCoefficient: TransferValueCoefficient,

                                                       allPaperBundles: PaperBundles[C],
                                                       bundlesToDistribute: ParSet[AssignedPaperBundle[C]],
                                                     ): BundleUpdate[C] = {
    val distributionOrigin = distributionReason match {
      case Election => PaperBundle.Origin.ElectedCandidate(candidateToDistribute, transferValueCoefficient, count)
      case Exclusion => PaperBundle.Origin.ExcludedCandidate(candidateToDistribute, count)
    }

    val newBundles = bundlesToDistribute.flatMap { bundle =>
      bundle.distributeToRemainingCandidates(
        distributionOrigin,
        count,
        candidateStatuses,
      )
    }

    val allBundlesIncludingNewOnes =
      (allPaperBundles diff bundlesToDistribute.asInstanceOf[ParSet[PaperBundle[C]]]) union newBundles

    BundleUpdate(newBundles, allBundlesIncludingNewOnes)
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

  // TODO this should only be computed once
  private def transferValueOf[C](paperBundles: ParSet[AssignedPaperBundle[C]]): TransferValue = {
    val (sumWeightXValue, sumWeight) = paperBundles
      .foldLeft((0d, 0l)) { case ((sumWeightXValue, sumWeight), bundle) =>
        (
          sumWeightXValue + (bundle.numPapers.asLong * bundle.transferValue.factor),
          sumWeight + bundle.numPapers.asLong,
        )
      }

    if (sumWeight == 0) {
      TransferValue(1)
    } else {
      TransferValue(sumWeightXValue / sumWeight)
    }
  }
}
