package au.id.tmm.countstv.utils

object PerCandidateCounts {

  def combine[C, T_VOTE_COUNT](
                                left: Map[C, T_VOTE_COUNT],
                                right: Map[C, T_VOTE_COUNT],
                              )(
                                op: (T_VOTE_COUNT, T_VOTE_COUNT) => T_VOTE_COUNT
                              ): Map[C, T_VOTE_COUNT] = {
    left.map { case (candidate, voteCountFromLeft) =>
      val voteCountFromThat = right(candidate)

      candidate -> op(voteCountFromLeft, voteCountFromThat)
    }
  }

}
