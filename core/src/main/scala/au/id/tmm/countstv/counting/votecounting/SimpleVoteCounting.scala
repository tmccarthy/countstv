package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.counting.PaperBundles
import au.id.tmm.countstv.model.VoteCount
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}

object SimpleVoteCounting {

  def performSimpleCount[C](
                             allCandidates: Set[C],
                             paperBundles: PaperBundles[C],
                           ): CandidateVoteCountsSansRoundingError[C] = {

    val interimCount = paperBundles
      .aggregate[InterimCount[C]](InterimCount[C]())(
      (countSoFar, bundle) => {
        val numVotes = bundle.transferValue.factor * bundle.numPapers.asLong

        bundle.assignedCandidate match {
          case Some(candidate) => {
            val oldVoteCount = countSoFar.perCandidate.getOrElse(candidate, InterimVoteCount())

            val newVoteCount = InterimVoteCount(
              numPapers = oldVoteCount.numPapers + bundle.numPapers.asLong,
              numVotes = oldVoteCount.numVotes + numVotes
            )

            val newPerCandidate = countSoFar.perCandidate.updated(candidate, newVoteCount)

            countSoFar.copy(perCandidate = newPerCandidate)
          }
          case None => {
            countSoFar.copy(
              exhausted = InterimVoteCount(
                numPapers = countSoFar.exhausted.numPapers + bundle.numPapers.asLong,
                numVotes = countSoFar.exhausted.numVotes + numVotes,
              )
            )
          }
        }
      },
      _ combineWith _,
    )

    interimCount.toCandidateVoteCounts(allCandidates)
  }

  private final case class InterimVoteCount(numPapers: Long = 0, numVotes: Double = 0d) {
    def combineWith(that: InterimVoteCount): InterimVoteCount =
      InterimVoteCount(this.numPapers + that.numPapers, this.numVotes + that.numVotes)

    def toVoteCount: VoteCount = VoteCount(NumPapers(numPapers), NumVotes.byRoundingDown(numVotes))
  }

  private final case class InterimCount[C](
                                            perCandidate: Map[C, InterimVoteCount] = Map.empty[C, InterimVoteCount],
                                            exhausted: InterimVoteCount = InterimVoteCount(),
                                          ) {

    def combineWith(that: InterimCount[C]): InterimCount[C] = {
      InterimCount(
        perCandidate =
          (this.perCandidate.keys ++ that.perCandidate.keys)
            .to(LazyList)
            .distinct
            .map { candidate =>
              val newInterimCount =
                this.perCandidate.getOrElse(candidate, InterimVoteCount()) combineWith
                  that.perCandidate.getOrElse(candidate, InterimVoteCount())

              candidate -> newInterimCount
            }
            .toMap,
        exhausted = this.exhausted combineWith that.exhausted,
      )
    }

    def toCandidateVoteCounts(allCandidates: Set[C]): CandidateVoteCountsSansRoundingError[C] =
      CandidateVoteCountsSansRoundingError(
        perCandidate = {
          allCandidates
            .map { candidate =>
              candidate -> this.perCandidate.getOrElse(candidate, InterimVoteCount()).toVoteCount
            }
            .toMap
        },
        exhausted = this.exhausted.toVoteCount,
      )
  }

}
