package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.{NewElectedCandidateComputations, VoteCounting}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{AllocationAfterIneligibles, CountContext}
import au.id.tmm.countstv.model.values.{Count, NumVotes}

object IneligibleHandling {

  def computeContextAfterIneligibles[C](previousContext: CountContext[C]): ProbabilityMeasure[CountContext[C]] = {
    val initialNumPapers = previousContext.numFormalPapers
    val numVacancies = previousContext.numVacancies
    val quota = previousContext.quota

    val initialAllocation = previousContext.mostRecentCountStep

    val oldCandidateStatuses = initialAllocation.candidateStatuses
    val oldVoteCounts = initialAllocation.candidateVoteCounts

    val allCandidates = oldCandidateStatuses.allCandidates
    val ineligibleCandidates = oldCandidateStatuses.ineligibleCandidates

    val oldPaperBundles = previousContext.paperBundles

    val newPaperBundlesPerIneligibleCandidate = oldPaperBundles
      .filter(b => b.assignedCandidate.exists(ineligibleCandidates.contains))
      .groupBy(b => b.assignedCandidate.get)
      .map { case (ineligibleCandidate, paperBundlesForIneligibleCandidate) =>
        val distributionOrigin = PaperBundle.Origin.IneligibleCandidate(ineligibleCandidate)

        val paperBundlesAfterDistribution = paperBundlesForIneligibleCandidate.flatMap { paperBundle =>
          paperBundle.distributeToRemainingCandidates(distributionOrigin, oldCandidateStatuses)
        }

          ineligibleCandidate -> paperBundlesAfterDistribution
      }

    val transfersDueToIneligibles = newPaperBundlesPerIneligibleCandidate
      .map { case (candidate, paperBundles) =>
          candidate -> VoteCounting.performSimpleCount(allCandidates, paperBundles)
      }

    val newPaperBundles = oldPaperBundles.filterNot(_.assignedCandidate.exists(ineligibleCandidates.contains)) ++
      newPaperBundlesPerIneligibleCandidate.values.flatten

    val newVoteCount = VoteCounting.countVotes(initialNumPapers, quota, oldCandidateStatuses, newPaperBundles)

    val newCandidateStatusPossibilities: ProbabilityMeasure[CandidateStatuses[C]] = computeCandidateStatusPossibilities(
      oldCandidateStatuses,
      quota,
      numVacancies,
      oldVoteCounts,
    )

    newCandidateStatusPossibilities.map { newCandidateStatuses =>
      val newCountStep = AllocationAfterIneligibles(
        candidateStatuses = newCandidateStatuses,
        candidateVoteCounts = newVoteCount,
        transfersDueToIneligibles = transfersDueToIneligibles,
      )

      previousContext.copy(
        paperBundles = newPaperBundles,
        previousCountSteps = previousContext.previousCountSteps :+ newCountStep,
      )
    }
  }

  private def computeCandidateStatusPossibilities[C](
                                                      oldCandidateStatuses: CandidateStatuses[C],
                                                      quota: NumVotes,
                                                      numVacancies: Int,
                                                      oldVoteCounts: CandidateVoteCounts[C],
                                                    ) = {
    val electedCandidatePossibilities = NewElectedCandidateComputations.newlyExceedingQuota(
      oldVoteCounts,
      previousCandidateVoteCountsAscending = List.empty,
      oldCandidateStatuses,
      numVacancies,
      quota,
    )

    val newCandidateStatusPossibilities = electedCandidatePossibilities.map { electedCandidates =>
      val statusPerElectedCandidate = electedCandidates
        .toStream
        .zipWithIndex
        .map { case (candidate, ordinalElected) =>
          candidate -> CandidateStatus.Elected(ordinalElected, Count.ofIneligibleCandidateHandling)
        }
        .toMap

      val statusPerCandidate: Map[C, CandidateStatus] = oldCandidateStatuses
        .allCandidates
        .toStream
        .map { candidate =>
          val newStatus = statusPerElectedCandidate.getOrElse(candidate, oldCandidateStatuses.asMap(candidate))

          candidate -> newStatus
        }
        .toMap

      CandidateStatuses[C](asMap = statusPerCandidate)
    }

    newCandidateStatusPossibilities
  }
}
