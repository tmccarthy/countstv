package au.id.tmm.countstv.counting

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}
import gnu.trove.map.hash.{TObjectDoubleHashMap, TObjectLongHashMap}

object VoteCounting {
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

  def performSimpleCount[C](
                             allCandidates: Set[C],
                             paperBundles: PaperBundles[C],
                           ): CandidateVoteCounts[C] = {

    val papersPerCandidate = new TObjectLongHashMap[C](allCandidates.size)
    val votesPerCandidate = new TObjectDoubleHashMap[C](allCandidates.size)

    allCandidates
      .foreach { candidate =>
        papersPerCandidate.put(candidate, 0)
        votesPerCandidate.put(candidate, 0)
      }

    var exhaustedVotes: VoteCount = VoteCount.zero

    for (bundle <- paperBundles) {
      val numVotes = bundle.transferValue * bundle.numPapers

      bundle.assignedCandidate match {
        case Some(assignedCandidate) => {
          papersPerCandidate.adjustValue(assignedCandidate, bundle.numPapers.asLong)
          votesPerCandidate.adjustValue(assignedCandidate, numVotes.asDouble)
        }
        case None => exhaustedVotes += VoteCount(bundle.numPapers, numVotes)
      }
    }

    val voteCountsPerCandidate = allCandidates
      .map { candidate =>
        val numPapers = NumPapers(papersPerCandidate.get(candidate))
        val numVotes = NumVotes(votesPerCandidate.get(candidate))
        val unroundedVoteCount = VoteCount(numPapers, numVotes)
        candidate -> roundVotes(unroundedVoteCount)
      }
      .toMap

    exhaustedVotes = roundVotes(exhaustedVotes)

    CandidateVoteCounts(
      perCandidate = voteCountsPerCandidate,
      exhausted = exhaustedVotes,
      roundingError = VoteCount.zero,
    )
  }

  private def roundVotes(voteCount: VoteCount): VoteCount = voteCount.copy(numVotes = voteCount.numVotes.roundedDown)
}
