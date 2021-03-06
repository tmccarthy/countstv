package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.CandidateDistributionReason.{Election, Exclusion}
import au.id.tmm.countstv.model.countsteps._
import au.id.tmm.countstv.model.values.{Count, NumVotes, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, CandidateVoteCounts}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.probability.measure.ProbabilityMeasure.Always
import au.id.tmm.probability.measure.{ProbabilityMeasure, TieSensitiveSorting}

object NextActionComputation {

  final case class NewStatusesAndNextAction[C, +T_ACTION <: CountAction[C]](
    candidateStatuses: CandidateStatuses[C],
    countAction: T_ACTION,
  )

  def computeNextAction[C](
    proposedCountSteps: CountSteps.Initial[C],
  ): ProbabilityMeasure.Always[NewStatusesAndNextAction[C, CountAction.AllocateAwayFromIneligibles.type]] =
    ProbabilityMeasure.Always(
      NewStatusesAndNextAction(
        proposedCountSteps.last.candidateStatuses,
        countAction = CountAction.AllocateAwayFromIneligibles,
      ),
    )

  def computeNextAction[C](
    numVacancies: Int,
    quota: NumVotes,
    proposedCountSteps: CountSteps.AllowingAppending[C],
  ): ProbabilityMeasure[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]] = {
    val count                = proposedCountSteps.last.count
    val oldCandidateStatuses = proposedCountSteps.last.candidateStatuses

    val currentVoteCounts                    = proposedCountSteps.last.candidateVoteCounts
    val previousCandidateVoteCountsAscending = proposedCountSteps.tail.dropRight(1).map(_.candidateVoteCounts).toList

    firstPresentIn[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]](
      List(
        () => Always(newStatusesAndActionIfCountFinished(numVacancies, oldCandidateStatuses)),
        () =>
          newStatusesAndActionIfRemainingCandidatesEqualsUnfilledVacancies(
            count,
            numVacancies,
            currentVoteCounts,
            previousCandidateVoteCountsAscending,
            oldCandidateStatuses) match {
            case Some(probabilityMeasure) => probabilityMeasure.map(Some(_))
            case None                     => Always(None)
          },
        () =>
          newStatusesAndActionFromDistributionOfElectedCandidate(
            count,
            currentVoteCounts,
            previousCandidateVoteCountsAscending,
            oldCandidateStatuses,
            numVacancies,
            quota,
            proposedCountSteps),
        () =>
          newStatusesAndActionIfTwoRemaining(
            count,
            numVacancies,
            currentVoteCounts,
            previousCandidateVoteCountsAscending,
            oldCandidateStatuses),
      ),
    ).flatMap {
      case Some(newStatusesAndNextAction) => Always(newStatusesAndNextAction)
      case None =>
        newStatusesAndActionFromExclusionOfCandidate(
          count,
          currentVoteCounts,
          previousCandidateVoteCountsAscending,
          oldCandidateStatuses)
    }
  }

  def computeShortCircuitingNextAction[C](
    numVacancies: Int,
    quota: NumVotes,
    proposedCountSteps: CountSteps.AllowingAppending[C],
  ): Option[ProbabilityMeasure[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]]] = {
    val count                = proposedCountSteps.last.count
    val oldCandidateStatuses = proposedCountSteps.last.candidateStatuses

    val currentVoteCounts                    = proposedCountSteps.last.candidateVoteCounts
    val previousCandidateVoteCountsAscending = proposedCountSteps.tail.dropRight(1).map(_.candidateVoteCounts).toList

    newStatusesAndActionIfCountFinished(numVacancies, oldCandidateStatuses).map(Always(_)) orElse
      newStatusesAndActionIfRemainingCandidatesEqualsUnfilledVacancies(
        count,
        numVacancies,
        currentVoteCounts,
        previousCandidateVoteCountsAscending,
        oldCandidateStatuses)
  }

  private def firstPresentIn[A](
    possibilities: List[() => ProbabilityMeasure[Option[A]]],
  ): ProbabilityMeasure[Option[A]] =
    possibilities match {
      case Nil => Always(None)
      case head :: tail =>
        head().flatMap {
          case Some(value) => Always(Some(value))
          case None        => firstPresentIn(tail)
        }
    }

  private def newStatusesAndActionIfCountFinished[C](
    numVacancies: Int,
    oldCandidateStatuses: CandidateStatuses[C],
  ): Option[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]] = {
    val numUnfilledVacancies = numVacancies - oldCandidateStatuses.electedCandidates.size

    if (numUnfilledVacancies == 0) {
      Some(NewStatusesAndNextAction(oldCandidateStatuses, CountAction.NoAction))
    } else {
      None
    }
  }

  private def newStatusesAndActionIfRemainingCandidatesEqualsUnfilledVacancies[C](
    count: Count,
    numVacancies: Int,
    currentCandidateVoteCounts: CandidateVoteCounts[C],
    previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
    oldCandidateStatuses: CandidateStatuses[C],
  ): Option[ProbabilityMeasure[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]]] = {
    val numUnfilledVacancies = numVacancies - oldCandidateStatuses.electedCandidates.size

    def ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)

    if (numUnfilledVacancies >= oldCandidateStatuses.remainingCandidates.size) {
      Some(
        TieSensitiveSorting
          .sort(oldCandidateStatuses.remainingCandidates)(ordering)
          .map(_.reverse.to(DupelessSeq))
          .map { electedCandidates =>
            val newStatuses = ElectedCandidateComputations
              .newCandidateStatusesAfterElectionOf(electedCandidates, count, oldCandidateStatuses)

            NewStatusesAndNextAction(newStatuses, CountAction.NoAction)
          },
      )
    } else {
      None
    }
  }

  private def newStatusesAndActionIfTwoRemaining[C](
    count: Count,
    numVacancies: Int,
    currentCandidateVoteCounts: CandidateVoteCounts[C],
    previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
    oldCandidateStatuses: CandidateStatuses[C],
  ): ProbabilityMeasure[Option[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]]] = {
    val numUnfilledVacancies = numVacancies - oldCandidateStatuses.electedCandidates.size

    def ordering = new CandidateVoteCountOrdering[C](currentCandidateVoteCounts, previousCandidateVoteCountsAscending)

    if (numUnfilledVacancies == 1 && oldCandidateStatuses.remainingCandidates.size == 2) {

      val electedCandidatePossibilities = TieSensitiveSorting
        .sort(oldCandidateStatuses.remainingCandidates)(ordering)
        .map(_.last)

      electedCandidatePossibilities.map { electedCandidate =>
        val newStatuses = ElectedCandidateComputations.newCandidateStatusesAfterElectionOf(
          DupelessSeq(electedCandidate),
          count,
          oldCandidateStatuses)

        Some(NewStatusesAndNextAction(newStatuses, CountAction.NoAction))
      }
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
        ElectedCandidateComputations.newCandidateStatusesAfterElectionOf(
          newlyElectedCandidates,
          count,
          oldCandidateStatuses)

      val numUnfilledVacancies = numVacancies - candidateStatusesAfterNewElections.electedCandidates.size

      if (numUnfilledVacancies == 0) {
        Some(NewStatusesAndNextAction(candidateStatusesAfterNewElections, CountAction.NoAction))
      } else {
        val candidatesAlreadyDistributed = proposedCountSteps.flatMap {
          case step: DistributionCountStep[C]     => Some(step.distributionSource.candidate)
          case step: ExcludedNoVotesCountStep[C]  => Some(step.excludedCandidate)
          case step: ElectedNoSurplusCountStep[C] => Some(step.electedCandidate)
          case _                                  => None
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
  ): ProbabilityMeasure[NewStatusesAndNextAction[C, CountAction.DuringDistribution[C]]] =
    ExcludedCandidateComputations
      .computeExcluded(currentCandidateVoteCounts, previousCandidateVoteCountsAscending, oldCandidateStatuses)
      .map { candidateToExclude =>
        val newStatusForCandidate = CandidateStatus.Excluded(
          ordinalExcluded = Ordinal.ofNextAdditionTo(oldCandidateStatuses.excludedCandidates),
          excludedAtCount = count,
        )

        val statusesAfterNewExclusion = oldCandidateStatuses.update(candidateToExclude, newStatusForCandidate)

        NewStatusesAndNextAction(
          statusesAfterNewExclusion,
          CountAction.DistributeFromCandidate(candidateToExclude, Exclusion))
      }

}
