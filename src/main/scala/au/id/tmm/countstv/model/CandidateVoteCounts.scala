package au.id.tmm.countstv.model

/**
  * A bundle of the [[VoteCount]] values per candidate, a count of exhausted votes, and a tally of any rounding error.
  */
final case class CandidateVoteCounts[C](
                                         perCandidate: Map[C, VoteCount],
                                         exhausted: VoteCount,
                                         roundingError: VoteCount,
                                       ) {

  def diff(that: CandidateVoteCounts[C]): CandidateVoteCounts[C] = {
    CandidateVoteCounts(
      perCandidate = this.perCandidate.map { case (candidate, voteCountFromThis) =>
        val voteCountFromThat = that.perCandidate(candidate)

        candidate -> (voteCountFromThis - voteCountFromThat)
      },
      exhausted = this.exhausted - that.exhausted,
      roundingError = this.roundingError - that.roundingError,
    )
  }

  def total: VoteCount = {
    val votesForCandidates = perCandidate.valuesIterator.reduceOption(_ + _).getOrElse(VoteCount.zero)

    votesForCandidates + exhausted + roundingError
  }
}
