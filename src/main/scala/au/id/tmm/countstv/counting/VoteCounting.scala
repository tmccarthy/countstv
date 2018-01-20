package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, CandidateVoteCounts, VoteCount}

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

    val votesPerCandidate: mutable.Map[C, VoteCount] = mutable.Map[C, VoteCount]()
    var exhaustedVotes: VoteCount = VoteCount.zero

    candidateStatuses
      .asMap
      .foreach { case (candidate, status) =>
        val voteCount = status match {
          case _: CandidateStatus.Elected => VoteCount(numPapers = 0, numVotes = quota)
          case _ => VoteCount.zero,
        }

        votesPerCandidate(candidate) = voteCount
      }

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
    val totalVoteCount = votesPerCandidate.valuesIterator.fold(VoteCount.zero)(_ + _)

    CandidateVoteCounts(
      perCandidate = votesPerCandidate.toMap,
      exhausted = exhaustedVotes,
      roundingError = (totalVoteCount + exhaustedVotes) - initialVoteCount,
    )
  }

  private def roundVotes(voteCount: VoteCount): VoteCount = voteCount.copy(numVotes = Math.floor(voteCount.numVotes))
}
