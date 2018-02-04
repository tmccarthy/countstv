package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.counting.{ElectedCandidateComputations, ExcludedCandidateComputations, VoteCounting}
import au.id.tmm.countstv.model.CandidateDistributionReason._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountContext, DistributionCountStep}
import au.id.tmm.countstv.model.values.{Count, NumPapers, NumVotes, TransferValueCoefficient}

import scala.collection.immutable.{Bag, HashedBagConfiguration, Queue}

object DistributiveCountStepComputation {

  def computeNextContext[C](countContext: CountContext[C]): ProbabilityMeasure[CountContext[C]] = {
    val count = countContext.mostRecentCountStep.count.increment
    val numFormalPapers = countContext.numFormalPapers
    val numVacancies = countContext.numVacancies
    val quota = countContext.quota
    val candidateStatuses = countContext.mostRecentCountStep.candidateStatuses
    val candidateVoteCounts = countContext.mostRecentCountStep.candidateVoteCounts
    val paperBundles = countContext.paperBundles
    val electedCandidatesWaitingToBeDistributed = countContext.electedCandidatesWaitingToBeDistributed
    val possibleCurrentDistribution = countContext.currentDistribution

    possibleCurrentDistribution match {
      case Some(currentDistribution) =>
        contextAfterApplyingDistribution(
          count,
          numFormalPapers,
          numVacancies,
          quota,
          candidateStatuses,
          paperBundles,
          currentDistribution,
        )

      case None =>
        if (electedCandidatesWaitingToBeDistributed.nonEmpty) {
          contextAfterDistributingElectedCandidate(
            count,
            numFormalPapers,
            numVacancies,
            quota,
            candidateStatuses,
            candidateVoteCounts,
            paperBundles,
            candidateToElect = electedCandidatesWaitingToBeDistributed.front,
          )
        } else {
          contextAfterDistributingExcludedCandidate(
            count,
            numFormalPapers,
            numVacancies,
            quota,
            candidateStatuses,
            candidateVoteCounts,
            paperBundles,
          )
        }
    }
  }

  private def contextAfterDistributingElectedCandidate[C](
                                                           count: Count,
                                                           numFormalPapers: NumPapers,
                                                           numVacancies: Int,
                                                           quota: NumVotes,
                                                           oldCandidateStatuses: CandidateStatuses[C],
                                                           oldCandidateVoteCounts: CandidateVoteCounts[C],
                                                           oldPaperBundles: PaperBundles[C],
                                                           candidateToElect: C,
                                                         ) = {
    val distributionTransferValue =
      TransferValueCoefficient.compute(oldCandidateVoteCounts.perCandidate(candidateToElect).numVotes, quota)

    val newCurrentDistribution =
      buildNewCurrentDistribution(
        candidateToElect,
        Election,
        distributionTransferValue,
        oldPaperBundles,
      )

    contextAfterApplyingDistribution(
      count,
      numFormalPapers,
      numVacancies,
      quota,
      oldCandidateStatuses,
      oldPaperBundles,
      newCurrentDistribution,
    )
  }

  private def contextAfterDistributingExcludedCandidate[C](
                                                            count: Count,
                                                            numFormalPapers: NumPapers,
                                                            numVacancies: Int,
                                                            quota: NumVotes,
                                                            oldCandidateStatuses: CandidateStatuses[C],
                                                            oldCandidateVoteCounts: CandidateVoteCounts[C],
                                                            oldPaperBundles: PaperBundles[C],
                                                          ) = {
    ExcludedCandidateComputations.computeExcluded(oldCandidateVoteCounts, oldCandidateStatuses)
      .flatMap { candidateToExclude =>

        val newCurrentDistribution =
          buildNewCurrentDistribution(
            candidateToExclude,
            Exclusion,
            distributionTransferValue = TransferValueCoefficient(1.0d),
            oldPaperBundles,
          )

        val statusForNewlyExcludedCandidate: CandidateStatus = {
          val ordinalExcluded = oldCandidateStatuses.excludedCandidates.size

          CandidateStatus.Excluded(ordinalExcluded, count)
        }

        val newCandidateStatuses =
          oldCandidateStatuses.update(candidateToExclude, statusForNewlyExcludedCandidate)

        contextAfterApplyingDistribution(
          count,
          numFormalPapers,
          numVacancies,
          quota,
          newCandidateStatuses,
          oldPaperBundles,
          newCurrentDistribution,
        )
      }
  }

  private def buildNewCurrentDistribution[C](
                                              candidateToDistribute: C,
                                              distributionReason: CandidateDistributionReason,
                                              distributionTransferValue: TransferValueCoefficient,
                                              oldPaperBundles: PaperBundles[C],
                                            ): CountContext.CurrentDistribution[C] = {
    val bundlesToDistribute = oldPaperBundles
      .toStream
      .collect {
        case b: AssignedPaperBundle[C] if b.assignedCandidate.contains(candidateToDistribute) => b
      }
      .groupBy(_.transferValue)
      .toStream
      .sortBy { case (transferValue, bundles) => transferValue }
      .map { case (transferValue, bundles) => bundles.to[Bag](Bag.canBuildFrom(HashedBagConfiguration.compact)) }
      .reverse
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
                                                   numFormalPapers: NumPapers,
                                                   numVacancies: Int,
                                                   quota: NumVotes,
                                                   oldCandidateStatuses: CandidateStatuses[C],
                                                   oldPaperBundles: PaperBundles[C],
                                                   oldCurrentDistribution: CountContext.CurrentDistribution[C],
                                                 ): ProbabilityMeasure[CountContext[C]] = {
    val candidateBeingDistributed = oldCurrentDistribution.candidateBeingDistributed
    val bundlesToDistribute = oldCurrentDistribution.bundlesToDistribute
    val distributionReason = oldCurrentDistribution.distributionReason

    val (bundlesToDistributeNow, remainingToDistributeForCurrentDistribution) =
      bundlesToDistribute.dequeue

    val newPaperBundles = {
      val distributionOrigin = distributionReason match {
        case Exclusion =>
          PaperBundle.Origin.ExcludedCandidate(candidateBeingDistributed, count)
        case Election =>
          PaperBundle.Origin.ElectedCandidate(candidateBeingDistributed, oldCurrentDistribution.transferValueCoefficient, count)
      }

      val newlyCreatedBundles = bundlesToDistributeNow
        .flatMap { bundle =>
          bundle.distributeToRemainingCandidates(
            distributionOrigin,
            oldCandidateStatuses,
          )
        }

      (oldPaperBundles -- bundlesToDistributeNow) ++ newlyCreatedBundles
    }

    val newCurrentDistribution = {
      if (remainingToDistributeForCurrentDistribution.nonEmpty) {
        Some(
          oldCurrentDistribution.copy(
            bundlesToDistribute = remainingToDistributeForCurrentDistribution
          )
        )
      } else {
        None
      }
    }

    val newCandidateVoteCounts = VoteCounting.countVotes(
      numFormalPapers,
      quota,
      oldCandidateStatuses,
      newPaperBundles,
    )

    val newDistributionCountStepSource = DistributionCountStep.Source(
      candidateBeingDistributed,
      distributionReason,
      sourceCounts = bundlesToDistributeNow
        .map(_.origin.count)
        .toSet,
      transferValue = oldCurrentDistribution.transferValueCoefficient * bundlesToDistributeNow.head.transferValue,
    )

    ElectedCandidateComputations.computeNewlyElected(
      newCandidateVoteCounts,
      oldCandidateStatuses,
      numVacancies,
      quota,
    )
      .map { newlyElectedCandidates =>
        val numCandidatesPreviouslyElected = oldCandidateStatuses.electedCandidates.size

        val statusesForNewlyElectedCandidates = newlyElectedCandidates
          .zipWithIndex
          .map { case (newlyElectedCandidate, indexElectedThisStep) =>
            newlyElectedCandidate -> (numCandidatesPreviouslyElected + indexElectedThisStep)
          }
          .map { case (newlyElectedCandidate, ordinalElected) =>
            newlyElectedCandidate -> CandidateStatus.Elected(ordinalElected, count)
          }
          .toMap

        val newCandidateStatuses = oldCandidateStatuses.updateFrom(statusesForNewlyElectedCandidates)

        val newCountStep = DistributionCountStep(
          count,
          newCandidateStatuses,
          newCandidateVoteCounts,
          newDistributionCountStepSource,
        )

        CountContext(
          numFormalPapers,
          numVacancies,
          newPaperBundles,
          newCountStep,
          newCurrentDistribution,
        )
      }
  }
}
