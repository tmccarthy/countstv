package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.{AssignedPaperBundle, ExhaustedPaperBundle, PaperBundle}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.values._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.parallel.immutable.ParSet

class SimpleVoteCountingSpec extends ImprovedFlatSpec {

  private val testPreferenceTree = PreferenceTree.from[Fruit](Set(Apple, Pear, Banana, Strawberry), numBallotsHint = 3)(List(
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Apple, Strawberry, Pear),
    Vector(Banana, Pear),
    Vector(Strawberry),
  ))

  "a simple vote count" should "correctly perform a simple count on a set of paper bundles" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Remaining,
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Remaining,
      Strawberry -> CandidateStatus.Remaining,
    )

    val paperBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, Count(2), candidateStatuses)

    val actualVoteCounts = SimpleVoteCounting.performSimpleCount[Fruit](
      allCandidates = candidateStatuses.allCandidates,
      paperBundles = paperBundles,
    )

    val expectedVoteCounts = CandidateVoteCountsSansRoundingError[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(3),
        Banana -> VoteCount(1),
        Pear -> VoteCount.zero,
        Strawberry -> VoteCount(1),
      ),
      exhausted = VoteCount.zero,
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

  it should "correctly perform a simple count on another set of paper bundles" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Remaining,
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Remaining,
      Strawberry -> CandidateStatus.Excluded(Ordinal.first, Count(1)),
    )

    val paperBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, Count(1), candidateStatuses)
      .flatMap { b =>
        b.distributeToRemainingCandidates(
          PaperBundle.Origin.ExcludedCandidate(Strawberry, Count(2)),
          Count(2),
          candidateStatuses
        )
      }

    val actualCount = SimpleVoteCounting.performSimpleCount(
      candidateStatuses.allCandidates,
      paperBundles,
    )

    val expectedCount = CandidateVoteCountsSansRoundingError(
      perCandidate = Map(
        Apple -> VoteCount(3),
        Banana -> VoteCount(1),
        Pear -> VoteCount.zero,
        Strawberry -> VoteCount.zero,
      ),
      exhausted = VoteCount(1),
    )

    assert(actualCount === expectedCount)
  }


  it should "correctly count the votes when some ballots have exhausted" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Elected(Ordinal.first, Count(1)),
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Excluded(Ordinal.first, Count(2)),
      Strawberry -> CandidateStatus.Excluded(Ordinal.second, Count(3)),
    )

    val paperBundles: ParSet[PaperBundle[Fruit]] = ParSet(
      AssignedPaperBundle(
        transferValue = TransferValue(0.666666666d),
        preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear, Banana).get,
        origin = PaperBundle.Origin.ElectedCandidate(Apple, TransferValue(0.666666666d), Count(2)),
      ),
      AssignedPaperBundle(
        transferValue = TransferValue(0.666666666d),
        preferenceTreeNode = testPreferenceTree.childFor(Apple, Banana).get,
        origin = PaperBundle.Origin.ElectedCandidate(Apple, TransferValue(0.666666666d), Count(2)),
      ),
      AssignedPaperBundle(
        transferValue = TransferValue(1d),
        preferenceTreeNode = testPreferenceTree.childFor(Banana).get,
        origin = PaperBundle.Origin.InitialAllocation,
      ),
      ExhaustedPaperBundle(
        numPapers = NumPapers(1),
        transferValue = TransferValue(1d),
        origin = PaperBundle.Origin.ExcludedCandidate(Strawberry, Count(2)),
        exhaustedAtCount = Count(2),
      ),
      ExhaustedPaperBundle(
        numPapers = NumPapers(1),
        transferValue = TransferValue(0.666666666d),
        origin = PaperBundle.Origin.ElectedCandidate(Apple, TransferValue(0.666666666d), Count(2)),
        exhaustedAtCount = Count(3),
      )
    )

    val actualVoteCounts = SimpleVoteCounting.performSimpleCount[Fruit](
      allCandidates = candidateStatuses.allCandidates,
      paperBundles = paperBundles,
    )

    val expectedVoteCounts = CandidateVoteCountsSansRoundingError[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(NumPapers(0), NumVotes(0)),
        Banana -> VoteCount(NumPapers(3), NumVotes(2)),
        Pear -> VoteCount(NumPapers(0), NumVotes(0)),
        Strawberry -> VoteCount(NumPapers(0), NumVotes(0)),
      ),
      exhausted = VoteCount(NumPapers(2), NumVotes(1)),
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

}
