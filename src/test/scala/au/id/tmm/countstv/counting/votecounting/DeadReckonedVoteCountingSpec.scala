package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.values._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.parallel.immutable.ParSet

class DeadReckonedVoteCountingSpec extends ImprovedFlatSpec {

  "a dead-reckoned count" should "be computed correctly" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Elected(Ordinal.first, Count(1)),
      Banana -> Remaining,
      Pear -> Remaining,
      Strawberry -> Remaining,
    )

    val preferenceTree = PreferenceTree.from[Fruit](
      Vector(Apple, Banana),
      Vector(Apple, Banana),
      Vector(Apple, Banana),
      Vector(Apple, Banana),
      Vector(Apple, Banana),
      Vector(Apple),
      Vector(Banana),
      Vector(Banana),
    )

    val numFormalPapers = preferenceTree.numPapers
    val numVacancies = 2
    val quota = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

    val oldVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(NumPapers(6), NumVotes(6)),
        Banana -> VoteCount(NumPapers(2), NumVotes(2)),
        Pear -> VoteCount(0),
        Strawberry -> VoteCount(0),
      ),
      exhausted = VoteCount(0),
      roundingError = VoteCount(0),
    )

    val removedBundles: ParSet[AssignedPaperBundle[Fruit]] = ParSet(
      AssignedPaperBundle[Fruit](
        TransferValue(1d),
        preferenceTree.childFor(Apple).get,
        PaperBundle.Origin.InitialAllocation,
      ),
    )

    val addedBundles: PaperBundles[Fruit] = ParSet(
      AssignedPaperBundle[Fruit](
        TransferValue(0.5d),
        preferenceTree.childFor(Apple, Banana).get,
        PaperBundle.Origin.ElectedCandidate(Apple, TransferValue(0.5d), Count(2)),
      ),
      ExhaustedPaperBundle[Fruit](
        NumPapers(1),
        TransferValue(0.5d),
        PaperBundle.Origin.ElectedCandidate(Apple, TransferValue(0.5d), Count(2)),
        exhaustedAtCount = Count(1),
      )
    )

    val actualVoteCounts = DeadReckonedVoteCounting.performDeadReckonedCount(
      numFormalPapers,
      quota,
      candidateStatuses,
      oldVoteCounts,
      removedBundles,
      addedBundles,
      TransferValue(0.5d),
    )

    val expectedVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(NumPapers(0), NumVotes(3)),
        Banana -> VoteCount(NumPapers(7), NumVotes(4)),
        Pear -> VoteCount(0),
        Strawberry -> VoteCount(0),
      ),
      exhausted = VoteCount(NumPapers(1), NumVotes(0)),
      roundingError = VoteCount(NumPapers(0), NumVotes(1)),
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

}
