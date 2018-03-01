package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.values._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class VoteCountingSpec extends ImprovedFlatSpec {

  private val testPreferenceTree = PreferenceTree.from[Fruit](
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Apple, Strawberry, Pear),
    Vector(Banana, Pear),
    Vector(Strawberry),
  )

  "a voteCount" should "correctly count the votes when no candidates are elected" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Remaining,
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Remaining,
      Strawberry -> CandidateStatus.Remaining,
    )

    val paperBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, candidateStatuses)

    val actualVoteCounts = VoteCounting.countVotes[Fruit](
      initialNumPapers = testPreferenceTree.numPapers,
      quota = NumVotes(2),
      candidateStatuses = candidateStatuses,
      paperBundles = paperBundles,
    )

    val expectedVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(3),
        Banana -> VoteCount(1),
        Pear -> VoteCount.zero,
        Strawberry -> VoteCount(1),
      ),
      exhausted = VoteCount.zero,
      roundingError = VoteCount.zero,
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

  it should "correctly count the votes when a candidate has been elected" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Elected(Ordinal.first, Count(1)),
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Remaining,
      Strawberry -> CandidateStatus.Remaining,
    )

    val transferValue = TransferValueCoefficient(0.666666666d)

    val paperBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, candidateStatuses)
      .flatMap { b =>
        b.distributeToRemainingCandidates(
          PaperBundle.Origin.ElectedCandidate(Apple, transferValue, Count(1)), candidateStatuses
        )
      }

    val actualVoteCounts = VoteCounting.countVotes[Fruit](
      initialNumPapers = testPreferenceTree.numPapers,
      quota = NumVotes(2),
      candidateStatuses = candidateStatuses,
      paperBundles = paperBundles,
    )

    val expectedVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(NumPapers(0), NumVotes(2)),
        Banana -> VoteCount(NumPapers(2), NumVotes(1)),
        Pear -> VoteCount(NumPapers(1), NumVotes(0)),
        Strawberry -> VoteCount(NumPapers(2), NumVotes(1)),
      ),
      exhausted = VoteCount.zero,
      roundingError = VoteCount(NumPapers(0), NumVotes(-1)),
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

  it should "correctly count the votes when a candidate has been elected but its ballots haven't been transferred" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Elected(Ordinal.first, Count(1)),
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Remaining,
      Strawberry -> CandidateStatus.Remaining,
    )

    val paperBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, candidateStatuses)

    val actualVoteCounts = VoteCounting.countVotes[Fruit](
      initialNumPapers = testPreferenceTree.numPapers,
      quota = NumVotes(2),
      candidateStatuses = candidateStatuses,
      paperBundles = paperBundles,
    )

    val expectedVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(3),
        Banana -> VoteCount(1),
        Pear -> VoteCount(0),
        Strawberry -> VoteCount(1),
      ),
      exhausted = VoteCount.zero,
      roundingError = VoteCount(0),
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

  it should "correctly count the votes when some ballots have exhausted" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Elected(Ordinal.first, Count(1)),
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Excluded(Ordinal.first, Count(2)),
      Strawberry -> CandidateStatus.Excluded(Ordinal.second, Count(3)),
    )

    val paperBundles: Set[PaperBundle[Fruit]] = Set(
      AssignedPaperBundle(
        transferValue = TransferValue(0.666666666d),
        preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear, Banana).get,
        origin = PaperBundle.Origin.ElectedCandidate(Apple, TransferValueCoefficient(0.666666666d), Count(2)),
      ),
      AssignedPaperBundle(
        transferValue = TransferValue(0.666666666d),
        preferenceTreeNode = testPreferenceTree.childFor(Apple, Banana).get,
        origin = PaperBundle.Origin.ElectedCandidate(Apple, TransferValueCoefficient(0.666666666d), Count(2)),
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
      ),
      ExhaustedPaperBundle(
        numPapers = NumPapers(1),
        transferValue = TransferValue(0.666666666d),
        origin = PaperBundle.Origin.ElectedCandidate(Apple, TransferValueCoefficient(0.666666666d), Count(2))
      )
    )

    val actualVoteCounts = VoteCounting.countVotes[Fruit](
      initialNumPapers = testPreferenceTree.numPapers,
      quota = NumVotes(2),
      candidateStatuses = candidateStatuses,
      paperBundles = paperBundles,
    )

    val expectedVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(NumPapers(0), NumVotes(2)),
        Banana -> VoteCount(NumPapers(3), NumVotes(2)),
        Pear -> VoteCount(NumPapers(0), NumVotes(0)),
        Strawberry -> VoteCount(NumPapers(0), NumVotes(0)),
      ),
      exhausted = VoteCount(NumPapers(2), NumVotes(1)),
      roundingError = VoteCount(NumPapers(0), NumVotes(0)),
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

  it should "correctly perform a simple count on a set of paper bundles" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Remaining,
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Remaining,
      Strawberry -> CandidateStatus.Excluded(Ordinal.first, Count(1)),
    )

    val paperBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, candidateStatuses)
      .flatMap { b =>
        b.distributeToRemainingCandidates(PaperBundle.Origin.ExcludedCandidate(Strawberry, Count(2)), candidateStatuses)
      }

    val actualCount = VoteCounting.performSimpleCount(candidateStatuses.allCandidates, paperBundles)

    val expectedCount = CandidateVoteCounts(
      perCandidate = Map(
        Apple -> VoteCount(3),
        Banana -> VoteCount(1),
        Pear -> VoteCount.zero,
        Strawberry -> VoteCount.zero,
      ),
      exhausted = VoteCount(1),
      roundingError = VoteCount.zero,
    )

    assert(actualCount === expectedCount)
  }
}
