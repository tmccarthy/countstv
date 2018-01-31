package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.{ElectedCandidateComputations, VoteCounting}
import au.id.tmm.countstv.model._

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
      previousContext.copy(
        paperBundles = newPaperBundles,
        mostRecentCountStep = AllocationAfterIneligibles(
          candidateStatuses = newCandidateStatuses,
          candidateVoteCounts = newVoteCount,
          transfersDueToIneligibles = transfersDueToIneligibles,
        ),
      )
    }
  }

  private def computeCandidateStatusPossibilities[C](
                                                      oldCandidateStatuses: CandidateStatuses[C],
                                                      quota: Long,
                                                      numVacancies: Int,
                                                      oldVoteCounts: CandidateVoteCounts[C],
                                                    ) = {
    val electedCandidatePossibilities = ElectedCandidateComputations.computeNewlyElected(
      oldVoteCounts,
      oldCandidateStatuses,
      numVacancies,
      quota,
    )

    val newCandidateStatusPossibilities = electedCandidatePossibilities.map { electedCandidates =>
      val statusPerElectedCandidate = electedCandidates
        .toStream
        .zipWithIndex
        .map { case (candidate, ordinalElected) =>
          candidate -> CandidateStatus.Elected(ordinalElected, CountNumbers.distributionOfIneligibleCandidates)
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
