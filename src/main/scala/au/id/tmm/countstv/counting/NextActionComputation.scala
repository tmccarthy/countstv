package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.CandidateDistributionReason.{Election, Exclusion}
import au.id.tmm.countstv.model.countsteps._
import au.id.tmm.countstv.model.values.{Count, NumVotes, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, CandidateVoteCounts}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.probabilities.ProbabilityMeasure.Always
import au.id.tmm.utilities.probabilities.{ProbabilityMeasure, TieSensitiveSorting}

object NextActionComputation {

  final case class NewStatusesAndNextAction[C, +T_ACTION <: CountAction[C]](candidateStatuses: CandidateStatuses[C], countAction: T_ACTION)

  def computeNextAction[C](
                            proposedCountSteps: CountSteps.Initial[C],
                          ): ProbabilityMeasure.Always[NewStatusesAndNextAction[C, CountAction.AllocateAwayFromIneligibles.type]] = {
    ProbabilityMeasure.Always(
      NewStatusesAndNextAction(
        proposedCountSteps.last.candidateStatuses,
        countAction = CountAction.AllocateAwayFromIneligibles,
      )
    )
  }

  def computeNextAction[C](
                            numVacancies: Int,
                            quota: NumVotes,
                            proposedCountSteps: CountSteps.AllowingAppending[C],
                          ): ProbabilityMeasure[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]] = {
    val count = proposedCountSteps.last.count
    val oldCandidateStatuses = proposedCountSteps.last.candidateStatuses

    val currentVoteCounts = proposedCountSteps.last.candidateVoteCounts
    val previousCandidateVoteCountsAscending = proposedCountSteps.tail.dropRight(1).map(_.candidateVoteCounts).toList

    newStatusesAndActionIfCountFinished(numVacancies, oldCandidateStatuses) orElse
      newStatusesAndActionIfFinalElectionPossible(numVacancies, currentVoteCounts, previousCandidateVoteCountsAscending, oldCandidateStatuses) orElse
      newStatusesAndActionIfCandidatesExceedQuota(count, currentVoteCounts, previousCandidateVoteCountsAscending, oldCandidateStatuses, numVacancies, quota, proposedCountSteps) getOrElse
      newStatusesAndActionFromExclusionOfCandidate(count, currentVoteCounts, previousCandidateVoteCountsAscending, oldCandidateStatuses)
  }

  def newStatusesAndActionIfCountFinished[C](
                                              numVacancies: Int,
                                              oldCandidateStatuses: CandidateStatuses[C],
                                            ): Option[ProbabilityMeasure[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]]] = {
    val numUnfilledVacancies = numVacancies - oldCandidateStatuses.electedCandidates.size

    if (numUnfilledVacancies == 0) {
      Some(ProbabilityMeasure.Always(NewStatusesAndNextAction(oldCandidateStatuses, CountAction.NoAction)))
    } else {
      None
    }
  }

  def newStatusesAndActionIfFinalElectionPossible[C](
                                                      numVacancies: Int,
                                                      currentCandidateVoteCounts: CandidateVoteCounts[C],
                                                      previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
                                                      oldCandidateStatuses: CandidateStatuses[C],
                                                    ): Option[ProbabilityMeasure[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]]] = {
    val numUnfilledVacancies = numVacancies - oldCandidateStatuses.electedCandidates.size

    if (numUnfilledVacancies == 1 && oldCandidateStatuses.remainingCandidates.size == 2) {

      val ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)

      val electedCandidatePossibilities = TieSensitiveSorting.sort(oldCandidateStatuses.remainingCandidates)(ordering)
        .map(_.last)

      Some(electedCandidatePossibilities.map { electedCandidate =>
        NewStatusesAndNextAction(oldCandidateStatuses, CountAction.MarkCandidateFinallyElected(electedCandidate))
      })
    } else if (numUnfilledVacancies == oldCandidateStatuses.remainingCandidates.size) {
      Some(ProbabilityMeasure.Always(NewStatusesAndNextAction(oldCandidateStatuses, CountAction.ElectAllRemainingCandidates)))
    } else {
      None
    }
  }

  def newStatusesAndActionIfCandidatesExceedQuota[C](
                                                      count: Count,
                                                      currentCandidateVoteCounts: CandidateVoteCounts[C],
                                                      previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
                                                      oldCandidateStatuses: CandidateStatuses[C],
                                                      numVacancies: Int,
                                                      quota: NumVotes,
                                                      proposedCountSteps: CountSteps.AllowingAppending[C],
                                                    ): Option[ProbabilityMeasure[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]]] = {
    val candidatesNewlyExceedingQuota = ElectedCandidateComputations.newlyExceedingQuota(
      currentCandidateVoteCounts,
      previousCandidateVoteCountsAscending,
      oldCandidateStatuses,
      numVacancies,
      quota,
    )

    candidatesNewlyExceedingQuota match {
      case Always(DupelessSeq()) => None
      case newlyElectedCandidates => Some {
        newlyElectedCandidates.map { newlyElectedCandidates =>
          val candidateStatusesAfterNewElections =
            ElectedCandidateComputations.newCandidateStatusesAfterElectionOf(newlyElectedCandidates, count, oldCandidateStatuses)

          val numUnfilledVacancies = numVacancies - candidateStatusesAfterNewElections.electedCandidates.size

          if (numUnfilledVacancies == 0) {
            NewStatusesAndNextAction(candidateStatusesAfterNewElections, CountAction.NoAction)
          } else {
            val candidatesAlreadyDistributed = proposedCountSteps.flatMap {
              case step: DistributionCountStep[C] => Some(step.distributionSource.candidate)
              case step: ExcludedNoVotesCountStep[C] => Some(step.excludedCandidate)
              case step: ElectedNoSurplusCountStep[C] => Some(step.electedCandidate)
              case _ => None
            }.toSet

            val nextElectedCandidateToDistribute = candidateStatusesAfterNewElections.electedCandidates
              .find(!candidatesAlreadyDistributed(_))
              .get

            NewStatusesAndNextAction(candidateStatusesAfterNewElections, CountAction.DistributeFromCandidate(nextElectedCandidateToDistribute, Election))
          }
        }
      }
    }
  }

  def newStatusesAndActionFromExclusionOfCandidate[C](
                                                       count: Count,
                                                       currentCandidateVoteCounts: CandidateVoteCounts[C],
                                                       previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
                                                       oldCandidateStatuses: CandidateStatuses[C],
                                                     ): ProbabilityMeasure[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]] = {
    ExcludedCandidateComputations.computeExcluded(currentCandidateVoteCounts, previousCandidateVoteCountsAscending, oldCandidateStatuses).map { candidateToExclude =>
      val newStatusForCandidate = CandidateStatus.Excluded(
        ordinalExcluded = Ordinal.ofNextAdditionTo(oldCandidateStatuses.excludedCandidates),
        excludedAtCount = count,
      )

      val statusesAfterNewExclusion = oldCandidateStatuses.update(candidateToExclude, newStatusForCandidate)

      NewStatusesAndNextAction(statusesAfterNewExclusion, CountAction.DistributeFromCandidate(candidateToExclude, Exclusion))
    }
  }

}
