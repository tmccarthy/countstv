package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.CandidateDistributionReason.{Election, Exclusion}
import au.id.tmm.countstv.model.countsteps._
import au.id.tmm.countstv.model.values.{Count, NumVotes, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, CandidateVoteCounts}
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

    firstPresentIn(
      Stream(
        newStatusesAndActionIfCountFinished(numVacancies, oldCandidateStatuses),
        newStatusesAndActionIfFinalElectionPossible(numVacancies, currentVoteCounts, previousCandidateVoteCountsAscending, oldCandidateStatuses),
        newStatusesAndActionFromDistributionOfElectedCandidate(count, currentVoteCounts, previousCandidateVoteCountsAscending, oldCandidateStatuses, numVacancies, quota, proposedCountSteps),
      ),
      newStatusesAndActionFromExclusionOfCandidate(count, currentVoteCounts, previousCandidateVoteCountsAscending, oldCandidateStatuses),
    )
  }

  private def firstPresentIn[A](
                                 possibilities: Stream[ProbabilityMeasure[Option[A]]],
                                 fallback: => ProbabilityMeasure[A],
                               ): ProbabilityMeasure[A] = {
    possibilities match {
      case Stream.Empty => fallback
      case head #:: tail => head.flatMap { possibility =>
        possibility.map(Always(_)).getOrElse(firstPresentIn(tail, fallback))
      }
    }
  }

  private def newStatusesAndActionIfCountFinished[C](
                                              numVacancies: Int,
                                              oldCandidateStatuses: CandidateStatuses[C],
                                            ): ProbabilityMeasure[Option[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]]] = Always {
    val numUnfilledVacancies = numVacancies - oldCandidateStatuses.electedCandidates.size

    if (numUnfilledVacancies == 0) {
      Some(NewStatusesAndNextAction(oldCandidateStatuses, CountAction.NoAction))
    } else {
      None
    }
  }

  // TODO final election can only occur if there are no elected candidates waiting for election
  // TODO final election is done at the end of a normal step
  private def newStatusesAndActionIfFinalElectionPossible[C](
                                                      numVacancies: Int,
                                                      currentCandidateVoteCounts: CandidateVoteCounts[C],
                                                      previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
                                                      oldCandidateStatuses: CandidateStatuses[C],
                                                    ): ProbabilityMeasure[Option[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]]] = {
    val numUnfilledVacancies = numVacancies - oldCandidateStatuses.electedCandidates.size

    if (numUnfilledVacancies == 1 && oldCandidateStatuses.remainingCandidates.size == 2) {

      val ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)

      val electedCandidatePossibilities = TieSensitiveSorting.sort(oldCandidateStatuses.remainingCandidates)(ordering)
        .map(_.last)

      electedCandidatePossibilities.map { electedCandidate =>
        Some(NewStatusesAndNextAction(oldCandidateStatuses, CountAction.MarkCandidateFinallyElected(electedCandidate)))
      }
    } else if (numUnfilledVacancies == oldCandidateStatuses.remainingCandidates.size) {
      Always(Some(NewStatusesAndNextAction(oldCandidateStatuses, CountAction.ElectAllRemainingCandidates)))
    } else {
      Always(None)
    }
  }

  private def newStatusesAndActionFromDistributionOfElectedCandidate[C](
                                                      count: Count,
                                                      currentCandidateVoteCounts: CandidateVoteCounts[C],
                                                      previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
                                                      oldCandidateStatuses: CandidateStatuses[C],
                                                      numVacancies: Int,
                                                      quota: NumVotes,
                                                      proposedCountSteps: CountSteps.AllowingAppending[C],
                                                    ): ProbabilityMeasure[Option[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]]] = {
    val candidatesNewlyExceedingQuota = ElectedCandidateComputations.newlyExceedingQuota(
      currentCandidateVoteCounts,
      previousCandidateVoteCountsAscending,
      oldCandidateStatuses,
      numVacancies,
      quota,
    )

    candidatesNewlyExceedingQuota.map { newlyElectedCandidates =>
      val candidateStatusesAfterNewElections =
        ElectedCandidateComputations.newCandidateStatusesAfterElectionOf(newlyElectedCandidates, count, oldCandidateStatuses)

      val numUnfilledVacancies = numVacancies - candidateStatusesAfterNewElections.electedCandidates.size

      if (numUnfilledVacancies == 0) {
        Some(NewStatusesAndNextAction(candidateStatusesAfterNewElections, CountAction.NoAction))
      } else {
        val candidatesAlreadyDistributed = proposedCountSteps.flatMap {
          case step: DistributionCountStep[C] => Some(step.distributionSource.candidate)
          case step: ExcludedNoVotesCountStep[C] => Some(step.excludedCandidate)
          case step: ElectedNoSurplusCountStep[C] => Some(step.electedCandidate)
          case _ => None
        }.toSet

        val nextElectedCandidateToDistribute = candidateStatusesAfterNewElections.electedCandidates
          .find(!candidatesAlreadyDistributed(_))

        nextElectedCandidateToDistribute.map { c =>
          NewStatusesAndNextAction(candidateStatusesAfterNewElections, CountAction.DistributeFromCandidate(c, Election))
        }
      }
    }
  }

  private def newStatusesAndActionFromExclusionOfCandidate[C](
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
