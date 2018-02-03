package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.{ElectedCandidateComputations, ExcludedCandidateComputations, VoteCounting}
import au.id.tmm.countstv.model.CandidateDistributionReason.{Election, Exclusion}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountContext, DistributionCountStep}
import au.id.tmm.countstv.{Count, PaperBundles}

import scala.collection.immutable.{Bag, HashedBagConfiguration, Queue}

object DistributiveCountStepComputation {

  def computeNextContext[C](countContext: CountContext[C]): ProbabilityMeasure[CountContext[C]] =
    computeNextContext(
      count = countContext.mostRecentCountStep.count + 1,
      numFormalPapers = countContext.numFormalPapers,
      numVacancies = countContext.numVacancies,
      quota = countContext.quota,
      oldCandidateStatuses = countContext.mostRecentCountStep.candidateStatuses,
      oldCandidateVoteCounts = countContext.mostRecentCountStep.candidateVoteCounts,
      oldPaperBundles = countContext.paperBundles,
      oldElectedCandidatesToBeDistributed = countContext.electedCandidatesToBeDistributed,
      possibleOldCurrentDistribution = countContext.currentDistribution,
    )

  private def computeNextContext[C](
                                     count: Count,

                                     numFormalPapers: Long,
                                     numVacancies: Int,
                                     quota: Long,

                                     oldCandidateStatuses: CandidateStatuses[C],
                                     oldCandidateVoteCounts: CandidateVoteCounts[C],

                                     oldPaperBundles: PaperBundles[C],

                                     oldElectedCandidatesToBeDistributed: Queue[C],
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
            ???
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
              DistributionCountStep.Source(
                candidateBeingDistributed,
                distributionReason,
                sourceCounts = bundlesToDistributeNow
                  .map(_.origin.count)
                  .toSet,
                1.0d,
              ),
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
        if (oldElectedCandidatesToBeDistributed.nonEmpty) {
          ???
        } else {
          ExcludedCandidateComputations.computeExcluded(oldCandidateVoteCounts, oldCandidateStatuses)
            .flatMap { candidateToExclude =>

              val bundlesToDistribute = oldPaperBundles
                .toStream
                .collect {
                  case b: AssignedPaperBundle[C] if b.assignedCandidate.contains(candidateToExclude) => b
                }
                .groupBy(_.transferValue)
                .toStream
                .sortBy { case (transferValue, bundles) => transferValue }
                .map { case (transferValue, bundles) => bundles.to[Bag](Bag.canBuildFrom(HashedBagConfiguration.compact)) }
                .to[Queue]

              val newCurrentDistribution = CountContext.CurrentDistribution[C](
                candidateBeingDistributed = candidateToExclude,
                distributionReason = CandidateDistributionReason.Exclusion,
                bundlesToDistribute = bundlesToDistribute,
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
                oldElectedCandidatesToBeDistributed,
                Some(newCurrentDistribution),
              )
            }
        }
      }
    }
  }

}
