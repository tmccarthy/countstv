package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.counting._
import au.id.tmm.countstv.model.CandidateDistributionReason._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountContext, DistributionCountStep}
import au.id.tmm.countstv.model.values.{Count, Ordinal, TransferValue, TransferValueCoefficient}
import au.id.tmm.utilities.collection.DupelessSeq

import scala.collection.immutable.Queue
import scala.collection.mutable

// TODO the previous counts used for tie-breaking shouldn't include counts where we're continuing with a previous
// distribution
object DistributiveCountStepComputation {

  def computeNextContext[C](countContext: CountContext[C]): ProbabilityMeasure[CountContext[C]] = {
    require(!countContext.allVacanciesNowFilled)

    val count = countContext.mostRecentCountStep.count.increment

    countContext.currentDistribution match {
      case Some(currentDistribution) => {
        contextAfterApplyingDistribution(
          count,
          countContext,
          currentDistribution,
        )
      }
      case None => {
        if (countContext.electedCandidatesWaitingToBeDistributed.nonEmpty) {
          contextAfterDistributingElectedCandidate(countContext)
        } else {
          ElectedCandidateComputations.finallyElected(
            countContext.previousCandidateVoteCounts.last,
            countContext.previousCandidateVoteCounts.init,
            countContext.candidateStatuses,
            countContext.numVacancies,
            countContext.quota,
          ).flatMap { finallyElected =>
            if (finallyElected.nonEmpty) {
              contextAfterFinalElectionOf(count, countContext, finallyElected)
            } else {
              contextAfterDistributingExcludedCandidate(count, countContext)
            }
          }
        }
      }
    }
  }

  private def contextAfterDistributingElectedCandidate[C](countContext: CountContext[C]) = {
    val candidateToElect = countContext.electedCandidatesWaitingToBeDistributed.front

    val distributionTransferValue =
      TransferValueCoefficient.compute(countContext.mostRecentCountStep.candidateVoteCounts.perCandidate(candidateToElect).numVotes, countContext.quota)

    val newCurrentDistribution =
      buildNewCurrentDistribution(
        candidateToElect,
        Election,
        distributionTransferValue,
        countContext.paperBundles,
      )

    computeNextContext(countContext.copy(currentDistribution = Some(newCurrentDistribution)))
  }

  private def contextAfterFinalElectionOf[C](
                                              count: Count,
                                              countContext: CountContext[C],
                                              finallyElected: DupelessSeq[C],
                                            ): ProbabilityMeasure[CountContext[C]] = {
    val newCandidateStatuses = newCandidateStatusesAfterElectionOf(finallyElected, count, countContext)

    val newCountStep = DistributionCountStep(
      count,
      newCandidateStatuses,
      countContext.mostRecentCountStep.candidateVoteCounts,
      distributionSource = None,
    )

    ProbabilityMeasure.always(
      countContext.copy(
        candidateStatuses = newCandidateStatuses,
        previousCountSteps = countContext.previousCountSteps :+ newCountStep,
        currentDistribution = None,
      )
    )
  }

  private def contextAfterDistributingExcludedCandidate[C](count: Count, countContext: CountContext[C]): ProbabilityMeasure[CountContext[C]] = {
    ExcludedCandidateComputations.computeExcluded(
      currentCandidateVoteCounts = countContext.previousCandidateVoteCounts.last,
      previousCandidateVoteCountsAscending = countContext.previousCandidateVoteCounts.init,
      candidateStatuses = countContext.candidateStatuses,
    )
      .flatMap { candidateToExclude =>
        val newCurrentDistribution =
          buildNewCurrentDistribution(
            candidateToExclude,
            Exclusion,
            distributionTransferValue = TransferValueCoefficient(1.0d),
            countContext.paperBundles,
          )

        val statusForNewlyExcludedCandidate: CandidateStatus = {
          val ordinalExcluded = Ordinal(countContext.candidateStatuses.excludedCandidates.size)

          CandidateStatus.Excluded(ordinalExcluded, count)
        }

        val newCandidateStatuses =
          countContext.candidateStatuses.update(candidateToExclude, statusForNewlyExcludedCandidate)

        computeNextContext(countContext.copy(
          candidateStatuses = newCandidateStatuses,
          currentDistribution = Some(newCurrentDistribution),
        ))
      }
  }


  private def buildNewCurrentDistribution[C](
                                              candidateToDistribute: C,
                                              distributionReason: CandidateDistributionReason,
                                              distributionTransferValue: TransferValueCoefficient,
                                              oldPaperBundles: PaperBundles[C],
                                            ): CountContext.CurrentDistribution[C] = {
    val bundlesPerTransferValue =
      new mutable.TreeMap[TransferValue, mutable.Set[AssignedPaperBundle[C]]]()(TransferValue.ordering.reverse)

    for (bundle <- oldPaperBundles) {
      bundle match {
        case b: AssignedPaperBundle[C] if b.assignedCandidate.contains(candidateToDistribute) =>
          bundlesPerTransferValue.getOrElseUpdate(b.transferValue, mutable.Set.empty) += b
        case _ =>
      }
    }

    val bundlesToDistribute = bundlesPerTransferValue
      .valuesIterator
      .map(_.toSet)
      .to[Queue]

    CountContext.CurrentDistribution[C](
      candidateBeingDistributed = candidateToDistribute,
      distributionReason = distributionReason,
      bundlesToDistribute = bundlesToDistribute,
      transferValueCoefficient = distributionTransferValue,
    )
  }

  private def contextAfterApplyingDistribution[C](
                                                   count: Count,
                                                   countContext: CountContext[C],
                                                   currentDistribution: CountContext.CurrentDistribution[C],
                                                 ): ProbabilityMeasure[CountContext[C]] = {
    val candidateBeingDistributed = currentDistribution.candidateBeingDistributed
    val bundlesToDistribute = currentDistribution.bundlesToDistribute
    val distributionReason = currentDistribution.distributionReason

    val (bundlesToDistributeNow, remainingToDistributeForCurrentDistribution) =
      bundlesToDistribute.dequeue

    val newPaperBundles = {
      val distributionOrigin = distributionReason match {
        case Exclusion =>
          PaperBundle.Origin.ExcludedCandidate(candidateBeingDistributed, count)
        case Election =>
          PaperBundle.Origin.ElectedCandidate(candidateBeingDistributed, currentDistribution.transferValueCoefficient, count)
      }

      val newlyCreatedBundles = bundlesToDistributeNow
        .flatMap { bundle =>
          bundle.distributeToRemainingCandidates(
            distributionOrigin,
            countContext.candidateStatuses,
          )
        }

      (countContext.paperBundles -- bundlesToDistributeNow) ++ newlyCreatedBundles
    }

    val newCurrentDistribution = {
      if (remainingToDistributeForCurrentDistribution.nonEmpty) {
        Some(
          currentDistribution.copy(
            bundlesToDistribute = remainingToDistributeForCurrentDistribution
          )
        )
      } else {
        None
      }
    }

    val newCandidateVoteCounts = VoteCounting.countVotes(
      countContext.numFormalPapers,
      countContext.quota,
      countContext.candidateStatuses,
      newPaperBundles,
    )

    val newDistributionCountStepSource = DistributionCountStep.Source(
      candidateBeingDistributed,
      distributionReason,
      sourceCounts = bundlesToDistributeNow
        .map(_.origin.count),
      transferValue = currentDistribution.transferValueCoefficient * bundlesToDistributeNow.head.transferValue,
    )

    ElectedCandidateComputations.newlyExceedingQuota(
      newCandidateVoteCounts,
      countContext.previousCandidateVoteCounts,
      countContext.candidateStatuses,
      countContext.numVacancies,
      countContext.quota,
    )
      .map { newlyElectedCandidates =>
        val numCandidatesPreviouslyElected = countContext.candidateStatuses.electedCandidates.size

        val statusesForNewlyElectedCandidates = newlyElectedCandidates
          .zipWithIndex
          .map { case (newlyElectedCandidate, indexElectedThisStep) =>
            newlyElectedCandidate -> (numCandidatesPreviouslyElected + indexElectedThisStep)
          }
          .map { case (newlyElectedCandidate, ordinalElected) =>
            newlyElectedCandidate -> CandidateStatus.Elected(Ordinal(ordinalElected), count)
          }
          .toMap

        val newCandidateStatuses = countContext.candidateStatuses.updateFrom(statusesForNewlyElectedCandidates)

        val newCountStep = DistributionCountStep(
          count,
          newCandidateStatuses,
          newCandidateVoteCounts,
          Some(newDistributionCountStepSource),
        )

        countContext.copy(
          paperBundles = newPaperBundles,
          previousCountSteps = countContext.previousCountSteps :+ newCountStep,
          candidateStatuses = newCandidateStatuses,
          currentDistribution = newCurrentDistribution,
        )
      }
  }

  private def newCandidateStatusesAfterElectionOf[C](
                                                      newlyElectedCandidates: DupelessSeq[C],
                                                      count: Count,
                                                      countContext: CountContext[C],
                                                    ): CandidateStatuses[C] = {
    val numCandidatesPreviouslyElected = countContext.candidateStatuses.electedCandidates.size

    val statusesForNewlyElectedCandidates = newlyElectedCandidates
      .zipWithIndex
      .map { case (newlyElectedCandidate, indexElectedThisStep) =>
        newlyElectedCandidate -> (numCandidatesPreviouslyElected + indexElectedThisStep)
      }
      .map { case (newlyElectedCandidate, ordinalElected) =>
        newlyElectedCandidate -> CandidateStatus.Elected(Ordinal(ordinalElected), count)
      }
      .toMap

    countContext.candidateStatuses.updateFrom(statusesForNewlyElectedCandidates)
  }
}
