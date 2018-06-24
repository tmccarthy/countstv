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
                                 ): (CandidateVoteCounts[C], TransferValue) = {

    // TODO probably do this concurrently
    val transferValue = VoteCountingUtilities.transferValueOf(addedBundles)
    val papersRemoved = countPapersFor(candidateStatuses.allCandidates, removedBundles.asInstanceOf[PaperBundles[C]])
    val papersAdded = countPapersFor(candidateStatuses.allCandidates, addedBundles)

    val papersTransferred = papersAdded - papersRemoved

    val votesTransferred = papersTransferred * transferValue

    val newVoteCount = CandidateVoteCountsSansRoundingError.from(previousVoteCounts) + votesTransferred

    val voteCountsAccountingForElections = VoteCountingUtilities.incorporateElectedCandidatesIntoCount(
      quota,
      newVoteCount,
      candidateStatuses,
    )

    val finalVoteCounts = VoteCountingUtilities.updateRoundingError(numFormalPapers, voteCountsAccountingForElections)

    (finalVoteCounts, transferValue)
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

}
