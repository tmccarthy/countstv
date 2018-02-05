package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.CandidateVoteCounts
import au.id.tmm.countstv.model.values.NumVotes

import scala.annotation.tailrec

private[counting] class CandidateVoteCountOrdering[C] (
                                                        currentCandidateVoteCounts: CandidateVoteCounts[C],
                                                        previousCandidateVoteCounts: Stream[CandidateVoteCounts[C]],
                                                      ) extends Ordering[C] {
  override def compare(left: C, right: C): Int = {
    compareUsingRecursively(Stream(currentCandidateVoteCounts) ++ previousCandidateVoteCounts)(left, right)
  }

  @tailrec
  private def compareUsingRecursively(counts: Stream[CandidateVoteCounts[C]])(left: C, right: C): Int = {
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
  }

  private def compareUsing(candidateVoteCounts: CandidateVoteCounts[C])(left: C, right: C): Int = {
    NumVotes.ordering.compare(
      candidateVoteCounts.perCandidate(left).numVotes,
      candidateVoteCounts.perCandidate(right).numVotes,
    )
  }
}

