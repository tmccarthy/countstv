package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.counting.{ElectedCandidateComputations, ExcludedCandidateComputations, VoteCounting}
import au.id.tmm.countstv.model.CandidateDistributionReason.{Election, Exclusion}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountContext, DistributionCountStep}
import au.id.tmm.countstv.model.values.{Count, NumPapers, NumVotes, TransferValueCoefficient}

import scala.collection.immutable.{Bag, HashedBagConfiguration, Queue}

// TODO refactor into smaller methods
object DistributiveCountStepComputation {

  def computeNextContext[C](countContext: CountContext[C]): ProbabilityMeasure[CountContext[C]] =
    computeNextContext(
      count = countContext.mostRecentCountStep.count.increment,
      numFormalPapers = countContext.numFormalPapers,
      numVacancies = countContext.numVacancies,
      quota = countContext.quota,
      oldCandidateStatuses = countContext.mostRecentCountStep.candidateStatuses,
      oldCandidateVoteCounts = countContext.mostRecentCountStep.candidateVoteCounts,
      oldPaperBundles = countContext.paperBundles,
      oldElectedCandidatesWaitingToBeDistributed = countContext.electedCandidatesWaitingToBeDistributed,
      possibleOldCurrentDistribution = countContext.currentDistribution,
    )

  private def computeNextContext[C](
                                     count: Count,

                                     numFormalPapers: NumPapers,
                                     numVacancies: Int,
                                     quota: NumVotes,

                                     oldCandidateStatuses: CandidateStatuses[C],
                                     oldCandidateVoteCounts: CandidateVoteCounts[C],

                                     oldPaperBundles: PaperBundles[C],

                                     oldElectedCandidatesWaitingToBeDistributed: Queue[C],
                                     possibleOldCurrentDistribution: Option[CountContext.CurrentDistribution[C]],
                                   ): ProbabilityMeasure[CountContext[C]] = {
    possibleOldCurrentDistribution match {
      case Some(oldCurrentDistribution) => {

        val candidateBeingDistributed = oldCurrentDistribution.candidateBeingDistributed
        val bundlesToDistribute = oldCurrentDistribution.bundlesToDistribute
        val distributionReason = oldCurrentDistribution.distributionReason

        val (bundlesToDistributeNow, remainingToDistributeForCurrentDistribution) =
          bundlesToDistribute.dequeue

        val distributionOrigin = distributionReason match {
          case Exclusion =>
            PaperBundle.Origin.ExcludedCandidate(candidateBeingDistributed, count)
          case Election =>
            PaperBundle.Origin.ElectedCandidate(candidateBeingDistributed, oldCurrentDistribution.transferValueCoefficient, count)
        }

        val newPaperBundles = {
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
      case None => {
        if (oldElectedCandidatesWaitingToBeDistributed.nonEmpty) {

          val (candidateToElect, newElectedCandidatesWaitingToBeDistributed) =
            oldElectedCandidatesWaitingToBeDistributed.dequeue

          val distributionTransferValue =
            TransferValueCoefficient.compute(oldCandidateVoteCounts.perCandidate(candidateToElect).numVotes, quota)

          val newCurrentDistribution =
            buildNewCurrentDistribution(
              candidateToElect,
              CandidateDistributionReason.Election,
              distributionTransferValue,
              oldPaperBundles,
            )

          computeNextContext(
            count,
            numFormalPapers,
            numVacancies,
            quota,
            oldCandidateStatuses,
            oldCandidateVoteCounts,
            oldPaperBundles,
            newElectedCandidatesWaitingToBeDistributed,
            Some(newCurrentDistribution),
          )

        } else {
          ExcludedCandidateComputations.computeExcluded(oldCandidateVoteCounts, oldCandidateStatuses)
            .flatMap { candidateToExclude =>

              val newCurrentDistribution =
                buildNewCurrentDistribution(
                  candidateToExclude,
                  CandidateDistributionReason.Exclusion,
                  distributionTransferValue = TransferValueCoefficient(1.0d),
                  oldPaperBundles,
                )

              val statusForNewlyExcludedCandidate: CandidateStatus = {
                val ordinalExcluded = oldCandidateStatuses.excludedCandidates.size

                CandidateStatus.Excluded(ordinalExcluded, count)
              }

              val newCandidateStatuses =
                oldCandidateStatuses.update(candidateToExclude, statusForNewlyExcludedCandidate)

              computeNextContext(
                count,
                numFormalPapers,
                numVacancies,
                quota,
                newCandidateStatuses,
                oldCandidateVoteCounts,
                oldPaperBundles,
                oldElectedCandidatesWaitingToBeDistributed,
                Some(newCurrentDistribution),
              )
            }
        }
      }
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
      .to[Queue]

    CountContext.CurrentDistribution[C](
      candidateBeingDistributed = candidateToDistribute,
      distributionReason = distributionReason,
      bundlesToDistribute = bundlesToDistribute,
      transferValueCoefficient = distributionTransferValue,
    )
  }
}
