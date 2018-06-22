package au.id.tmm.countstv.model

/**
  * A bundle of the [[VoteCount]] values per candidate, a count of exhausted votes, and a tally of any rounding error.
  */
final case class CandidateVoteCounts[C](
                                         perCandidate: Map[C, VoteCount],
                                         exhausted: VoteCount,
                                         roundingError: VoteCount,
                                       ) {

  def + (that: CandidateVoteCounts[C]): CandidateVoteCounts[C] = this.combineWith(that)(_ + _)
  def - (that: CandidateVoteCounts[C]): CandidateVoteCounts[C] = this.combineWith(that)(_ - _)

  private def combineWith(that: CandidateVoteCounts[C])(op: (VoteCount, VoteCount) => VoteCount): CandidateVoteCounts[C] = {
    CandidateVoteCounts(
      perCandidate = this.perCandidate.map { case (candidate, voteCountFromThis) =>
        val voteCountFromThat = that.perCandidate(candidate)

        candidate -> op(voteCountFromThis, voteCountFromThat)
      },
      exhausted = op(this.exhausted, that.exhausted),
      roundingError = op(this.roundingError, that.roundingError),
    )
  }

  def totalVotesForCandidates: VoteCount = perCandidate.valuesIterator.reduceOption(_ + _).getOrElse(VoteCount.zero)

  def total: VoteCount = {
    totalVotesForCandidates + exhausted + roundingError
  }
}
