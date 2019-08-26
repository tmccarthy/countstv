package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.model.{CandidateVoteCounts, VoteCount}
import au.id.tmm.countstv.utils.PerCandidateCounts

// TODO find the correct package for this
// TODO this needs a better name
final case class CandidateVoteCountsSansRoundingError[C](perCandidate: Map[C, VoteCount], exhausted: VoteCount) {
  def total: VoteCount = perCandidate.values.reduceOption(_ + _).getOrElse(VoteCount(0)) + exhausted

  def withRoundingError(roundingError: VoteCount): CandidateVoteCounts[C] =
    CandidateVoteCounts[C](
      perCandidate = this.perCandidate,
      exhausted = this.exhausted,
      roundingError = roundingError,
    )

  def +(that: CandidateVoteCountsSansRoundingError[C]): CandidateVoteCountsSansRoundingError[C] =
    CandidateVoteCountsSansRoundingError(
      perCandidate = PerCandidateCounts.combine(this.perCandidate, that.perCandidate)(_ + _),
      exhausted = this.exhausted + that.exhausted,
    )
}

object CandidateVoteCountsSansRoundingError {
  def from[C](withRoundingError: CandidateVoteCounts[C]): CandidateVoteCountsSansRoundingError[C] =
    CandidateVoteCountsSansRoundingError(
      perCandidate = withRoundingError.perCandidate,
      exhausted = withRoundingError.exhausted,
    )
}
