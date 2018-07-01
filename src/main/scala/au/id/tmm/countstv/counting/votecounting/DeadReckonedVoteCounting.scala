package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.counting.{AssignedPaperBundle, PaperBundles}
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes, TransferValue}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts}

import scala.collection.parallel.immutable.ParSet

object DeadReckonedVoteCounting {

  def performDeadReckonedCount[C](
                                   numFormalPapers: NumPapers,
                                   quota: NumVotes,
                                   candidateStatuses: CandidateStatuses[C],
                                   previousVoteCounts: CandidateVoteCounts[C],
                                   removedBundles: ParSet[AssignedPaperBundle[C]],
                                   addedBundles: PaperBundles[C],
                                   transferValue: TransferValue,
                                 ): CandidateVoteCounts[C] = {

    // TODO probably do this concurrently
    val papersRemoved = countPapersFor(candidateStatuses.allCandidates, removedBundles.asInstanceOf[PaperBundles[C]])
    val papersAdded = countPapersFor(candidateStatuses.allCandidates, addedBundles)

    val papersTransferred = papersAdded - papersRemoved

    val votesTransferred = papersTransferred * transferValue

    val newVoteCount = ensureAllVoteCountsNonNegative(CandidateVoteCountsSansRoundingError.from(previousVoteCounts) + votesTransferred)

    val voteCountsAccountingForElections = VoteCountingUtilities.incorporateElectedCandidatesIntoCount(
      quota,
      newVoteCount,
      candidateStatuses,
    )

    val finalVoteCounts = VoteCountingUtilities.updateRoundingError(numFormalPapers, voteCountsAccountingForElections)

    finalVoteCounts
  }

  private def countPapersFor[C](
                                 allCandidates: Set[C],
                                 paperBundles: PaperBundles[C],
                               ): CandidatePaperCounts[C] = {
    paperBundles.aggregate(CandidatePaperCounts.zeroForEachOf(allCandidates))(
      seqop = { case (paperCounts, paperBundle) =>
        paperBundle.assignedCandidate match {
          case Some(c) => paperCounts.increment(c, paperBundle.numPapers)
          case None => paperCounts.copy(exhausted = paperCounts.exhausted + paperBundle.numPapers)
        }
      },
      combop = { case (left, right) =>
        left + right
      },
    )
  }

  // In some rare cases, due to rounding errors, vote counts can end up negative under the rounding rules that work
  // in normal circumstances. We handle this here.
  private def ensureAllVoteCountsNonNegative[C](
                                                 voteCounts: CandidateVoteCountsSansRoundingError[C],
                                               ): CandidateVoteCountsSansRoundingError[C] = {
    val someCandidatesHaveNegativeVotes = voteCounts.perCandidate.exists { case (_, voteCount) =>
        voteCount.numVotes < NumVotes(0)
    }

    if (someCandidatesHaveNegativeVotes || voteCounts.exhausted.numVotes < NumVotes(0)) {
      voteCounts.copy(
        perCandidate = voteCounts.perCandidate.mapValues { voteCount =>
          if (voteCount.numVotes < NumVotes(0)) voteCount.copy(numVotes = NumVotes(0)) else voteCount
        },
        exhausted = voteCounts.exhausted.copy(numVotes = NumVotes(0)),
      )
    } else {
      voteCounts
    }
  }

}
