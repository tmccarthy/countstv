package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model._

import scala.collection.immutable.Bag
import scala.collection.mutable

object VoteCounting {
  def countVotes[C](
                     initialNumPapers: Long,
                     quota: Long,
                     candidateStatuses: CandidateStatuses[C],
                     paperBundles: Bag[PaperBundle[C]],
                   ): CandidateVoteCounts[C] = {
    val initialVoteCount = VoteCount(numPapers = initialNumPapers, numVotes = initialNumPapers)

    val simpleCount = performSimpleCount(candidateStatuses.allCandidates, paperBundles)

    val countIncorporatingElectedCandidates = simpleCount.copy(
      perCandidate = simpleCount.perCandidate.map { case (candidate, voteCountFromSimpleCount) =>

        val candidateStatus = candidateStatuses.asMap(candidate)

        val voteCountForCandidate = {
          if (candidateStatus.isInstanceOf[CandidateStatus.Elected] && voteCountFromSimpleCount == VoteCount.zero) {
            voteCountFromSimpleCount + VoteCount(numPapers = 0, numVotes = quota)
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

  def performSimpleCount[C](
                             allCandidates: Set[C],
                             paperBundles: Bag[PaperBundle[C]],
                           ): CandidateVoteCounts[C] = {

    val votesPerCandidate: mutable.Map[C, VoteCount] = mutable.Map(
      allCandidates
        .toStream
        .map(c => c -> VoteCount.zero)
        : _*
    )

    var exhaustedVotes: VoteCount = VoteCount.zero

    paperBundles
      .toIterator
      .foreach { b: PaperBundle[C] =>
        val numVotes = b.transferValue * b.numPapers

        val voteCount = VoteCount(
          numPapers = b.numPapers,
          numVotes = numVotes,
        )

        b.assignedCandidate match {
          case Some(assignedCandidate) => votesPerCandidate(assignedCandidate) += voteCount
          case None => exhaustedVotes += voteCount
        }
      }

    votesPerCandidate.transform { case (_, voteCount) =>
      roundVotes(voteCount)
    }

    exhaustedVotes = roundVotes(exhaustedVotes)

    CandidateVoteCounts(
      perCandidate = votesPerCandidate.toMap,
      exhausted = exhaustedVotes,
      roundingError = VoteCount.zero,
    )
  }

  private def roundVotes(voteCount: VoteCount): VoteCount = voteCount.copy(numVotes = Math.floor(voteCount.numVotes))
}
