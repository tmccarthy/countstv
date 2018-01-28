package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model._

import scala.collection.immutable.Bag

object IneligibleHandling {
  def handleIneligibleCandidates[C](
                                     initialAllocation: InitialAllocation[C],
                                     initialNumPapers: Long,
                                     quota: Long,
                                     numVacancies: Int,
                                     oldPaperBundles: Bag[PaperBundle[C]],
                                   ): ProbabilityMeasure[AllocationAfterIneligibles[C]] = {
    val oldCandidateStatuses = initialAllocation.candidateStatuses
    val oldVoteCounts = initialAllocation.candidateVoteCounts

    val allCandidates = oldCandidateStatuses.allCandidates
    val ineligibleCandidates = oldCandidateStatuses.ineligibleCandidates

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
      AllocationAfterIneligibles(
        candidateStatuses = newCandidateStatuses,
        candidateVoteCounts = newVoteCount,
        transfersDueToIneligibles = transfersDueToIneligibles,
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
