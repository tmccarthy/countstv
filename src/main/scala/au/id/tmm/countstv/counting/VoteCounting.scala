package au.id.tmm.countstv.counting

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}

object VoteCounting {

  /**
    * Produces a count of votes per candidate, as well as a count of exhausted votes, and any error due to rounding. Any
    * elected candidates, despite necessarily having no papers allocated to them in the count, are counted as having a
    * quota of votes.
    */
  def countVotes[C](
                     initialNumPapers: NumPapers,
                     quota: NumVotes,
                     candidateStatuses: CandidateStatuses[C],
                     paperBundles: PaperBundles[C],
                   ): CandidateVoteCounts[C] = {
    val initialVoteCount = VoteCount(initialNumPapers.asLong)

    val simpleCount = performSimpleCount(candidateStatuses.allCandidates, paperBundles)

    val countIncorporatingElectedCandidates = simpleCount.copy(
      perCandidate = simpleCount.perCandidate.map { case (candidate, voteCountFromSimpleCount) =>

        val candidateStatus = candidateStatuses.asMap(candidate)

        val voteCountForCandidate = {
          // TODO I don't think this is correct *during* a distribution
          if (candidateStatus.isInstanceOf[CandidateStatus.Elected] && voteCountFromSimpleCount == VoteCount.zero) {
            voteCountFromSimpleCount + VoteCount(NumPapers(0), quota)
          } else {
            voteCountFromSimpleCount
          }
        }

        candidate -> voteCountForCandidate
      }
    )

    val totalVoteCount = countIncorporatingElectedCandidates.perCandidate.valuesIterator.fold(VoteCount.zero)(_ + _)
    val roundingError = (totalVoteCount + countIncorporatingElectedCandidates.exhausted) - initialVoteCount

    countIncorporatingElectedCandidates.copy(
      roundingError = roundingError
    )
  }

  /**
    * Produces a count of votes per candidate, as well as a count of exhausted votes. Unlike
    * `performCount()`, this method does not track rounding error. Nor does it allocate a quota of votes to any elected
    * candidates.
    */
  def performSimpleCount[C](
                             allCandidates: Set[C],
                             paperBundles: PaperBundles[C],
                           ): CandidateVoteCounts[C] = {

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
            .toStream
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

    def toCandidateVoteCounts(allCandidates: Set[C]): CandidateVoteCounts[C] = CandidateVoteCounts(
      perCandidate = {
        allCandidates
          .map { candidate =>
            candidate -> this.perCandidate.getOrElse(candidate, InterimVoteCount()).toVoteCount
          }
          .toMap
      },
      exhausted = this.exhausted.toVoteCount,
      roundingError = VoteCount.zero,
    )

  }
}
