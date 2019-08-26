package au.id.tmm.countstv.model

import au.id.tmm.countstv.utils.PerCandidateCounts

/**
  * A bundle of the [[VoteCount]] values per candidate, a count of exhausted votes, and a tally of any rounding error.
  */
final case class CandidateVoteCounts[C](
  perCandidate: Map[C, VoteCount],
  exhausted: VoteCount,
  roundingError: VoteCount,
) {

  def +(that: CandidateVoteCounts[C]): CandidateVoteCounts[C] = this.combineWith(that)(_ + _)
  def -(that: CandidateVoteCounts[C]): CandidateVoteCounts[C] = this.combineWith(that)(_ - _)

  private def combineWith(
    that: CandidateVoteCounts[C],
  )(
    op: (VoteCount, VoteCount) => VoteCount,
  ): CandidateVoteCounts[C] =
    CandidateVoteCounts(
      perCandidate = PerCandidateCounts.combine(this.perCandidate, that.perCandidate)(op),
      exhausted = op(this.exhausted, that.exhausted),
      roundingError = op(this.roundingError, that.roundingError),
    )

  def totalVotesForCandidates: VoteCount = perCandidate.valuesIterator.reduceOption(_ + _).getOrElse(VoteCount.zero)

  def total: VoteCount =
    totalVotesForCandidates + exhausted + roundingError
}
