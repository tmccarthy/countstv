package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.CandidateVoteCounts
import au.id.tmm.countstv.model.values.NumVotes

import scala.annotation.tailrec

/**
  * An ordering of candidates, based on their current vote counts, and vote counts from previous steps. The counts from
  * previous steps are used to break ties.
  */
class CandidateVoteCountOrdering[C](
  currentCandidateVoteCounts: CandidateVoteCounts[C],
  previousCandidateVoteCountsAscending: List[CandidateVoteCounts[C]],
) extends Ordering[C] {
  override def compare(left: C, right: C): Int = {
    val allVoteCountsDescending = List(currentCandidateVoteCounts) ++ previousCandidateVoteCountsAscending.reverse
    compareUsingRecursively(allVoteCountsDescending)(left, right)
  }

  @tailrec
  private def compareUsingRecursively(counts: List[CandidateVoteCounts[C]])(left: C, right: C): Int =
    counts.headOption match {
      case Some(candidateVoteCounts) => {
        val comparison = compareUsing(candidateVoteCounts)(left, right)

        if (comparison != 0) {
          comparison
        } else {
          compareUsingRecursively(counts.tail)(left, right)
        }
      }
      case None => 0
    }

  private def compareUsing(candidateVoteCounts: CandidateVoteCounts[C])(left: C, right: C): Int =
    NumVotes.ordering.compare(
      candidateVoteCounts.perCandidate(left).numVotes,
      candidateVoteCounts.perCandidate(right).numVotes,
    )
}
