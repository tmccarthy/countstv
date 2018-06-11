package au.id.tmm.countstv.counting.render

import au.id.tmm.countstv.counting.render.CountStepRenderer.StepCandidate.{Candidate, Exhausted, RoundingError}
import au.id.tmm.countstv.model.CandidateStatus.Remaining
import au.id.tmm.countstv.model.countsteps.DistributionCountStep._
import au.id.tmm.countstv.model.countsteps._
import au.id.tmm.countstv.model.values.{Count, NumPapers, NumVotes, TransferValue}
import au.id.tmm.countstv.model.{CandidateDistributionReason, CandidateStatus, VoteCount}
import au.id.tmm.utilities.collection.DupelessSeq

/**
  * Renders count steps into a structure similar to that used by the Australian Electoral Commission to publish count
  * results
  */
object CountStepRenderer {

  def renderRowsFor[C](
                        numVacancies: Int,
                        totalFormalPapers: NumPapers,
                        quota: NumVotes,
                      )
                      (
                        previousCountStep: Option[CountStep[C]],
                        countStep: CountStep[C],
                        nextCountStep: Option[CountStep[C]],
                      )(implicit
                        ordering: Ordering[C],
                      ): List[RenderedRow[C]] = {

    val stepComment = nextCountStep match {
      case Some(_: InitialAllocation[C]) =>
        StepComment.InitialAllocation

      case Some(_: AllocationAfterIneligibles[C]) =>
        StepComment.InitialAllocation

      case Some(DistributionCountStep(count, _, _, Source(candidate, reason, sourceCounts, transferValue))) =>
        StepComment.NextStepDistributing(candidate, reason, count, transferValue, sourceCounts)

      case Some(ExcludedNoVotesCountStep(count, _, _, excludedCandidate)) =>
        StepComment.ExcludedNoVotes(excludedCandidate, count)

      case Some(ElectedNoSurplusCountStep(count, _, _, electedCandidate, sourceCounts)) =>
        StepComment.ElectedNoSurplus(electedCandidate, count, sourceCounts)

      case Some(FinalElectionCountStep(count, _, _, electedCandidates)) =>
        StepComment.FinalElection(electedCandidates, count)

      case None =>
        StepComment.AllVacanciesFilled
    }

    def renderedRow(candidate: StepCandidate[C]): RenderedRow[C] = {
      val previousVoteCount = previousCountStep.map(voteCountFor(_)(candidate)).getOrElse(VoteCount.zero)
      val thisVoteCount = voteCountFor(countStep)(candidate)

      val transferredThisCount = thisVoteCount - previousVoteCount

      val previousStatus = previousCountStep.map(statusFor(_)(candidate)).getOrElse(Remaining)
      val thisStatus = statusFor(countStep)(candidate)

      RenderedRow(
        numVacancies,
        totalFormalPapers,
        quota,
        countStep.count,
        candidate,
        transferredThisCount,
        thisVoteCount,
        transferValue = countStep match {
          case c: DistributionCountStep[C] => c.distributionSource.transferValue
          case _ => TransferValue(1)
        },
        statusFor(countStep)(candidate),
        changedThisStep = thisStatus != previousStatus, // TODO
        stepComment,
      )
    }

    val candidateRows = countStep.candidateStatuses
      .allCandidates
      .toList
      .sorted
      .map(c => renderedRow(Candidate(c)))

    candidateRows :+ renderedRow(Exhausted) :+ renderedRow(RoundingError)
  }

  private def voteCountFor[C](countStep: CountStep[C])(candidate: StepCandidate[C]) = candidate match {
    case Candidate(c) => countStep.candidateVoteCounts.perCandidate(c)
    case Exhausted => countStep.candidateVoteCounts.exhausted
    case RoundingError => countStep.candidateVoteCounts.roundingError
  }

  private def statusFor[C](countStep: CountStep[C])(candidate: StepCandidate[C]) = candidate match {
    case Candidate(c) => countStep.candidateStatuses.asMap(c)
    case Exhausted | RoundingError => Remaining
  }

  final case class RenderedRow[+C](
                                    numVacancies: Int,
                                    totalFormalPapers: NumPapers,
                                    quota: NumVotes,
                                    count: Count,
                                    candidate: StepCandidate[C],
                                    votesTransferred: VoteCount,
                                    progressiveVoteTotal: VoteCount,
                                    transferValue: TransferValue,
                                    status: CandidateStatus,
                                    changedThisStep: Boolean,
                                    stepComment: StepComment[C],
                                  )

  sealed trait StepComment[+C]
  object StepComment {
    case object InitialAllocation extends StepComment[Nothing]
    case class NextStepDistributing[C](candidate: C, reason: CandidateDistributionReason, nextCount: Count, transferValue: TransferValue, sourceCounts: Set[Count]) extends StepComment[C]
    case class ExcludedNoVotes[C](candidate: C, nextCount: Count) extends StepComment[C]
    case class ElectedNoSurplus[C](candidate: C, nextCount: Count, sourceCounts: Set[Count]) extends StepComment[C]
    case class FinalElection[C](candidates: DupelessSeq[C], nextCount: Count) extends StepComment[C]
    case object AllVacanciesFilled extends StepComment[Nothing]
  }

  sealed trait StepCandidate[+C]
  object StepCandidate {
    case object Exhausted extends StepCandidate[Nothing]
    case object RoundingError extends StepCandidate[Nothing]
    case class Candidate[C](candidate: C) extends StepCandidate[C]
  }
}
