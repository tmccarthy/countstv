package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.immutable.{Bag, BagConfiguration}

class VoteCountingSpec extends ImprovedFlatSpec {

  private implicit val bagConfiguration: BagConfiguration[PaperBundle[Fruit]] = PaperBundle.bagConfiguration[Fruit]

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
      quota = 2l,
      candidateStatuses = candidateStatuses,
      paperBundles = paperBundles,
    )

    val expectedVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(numPapers = 3, numVotes = 3),
        Banana -> VoteCount(numPapers = 1, numVotes = 1),
        Pear -> VoteCount.zero,
        Strawberry -> VoteCount(numPapers = 1, numVotes = 1),
      ),
      exhausted = VoteCount.zero,
      roundingError = VoteCount.zero,
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

  it should "correctly count the votes when a candidate has been elected" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Elected(ordinalElected = 0, electedAtCount = 1),
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Remaining,
      Strawberry -> CandidateStatus.Remaining,
    )

    val transferValue = 0.666666666d

    val paperBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, candidateStatuses)
      .flatMap { b =>
        b.distributeToRemainingCandidates(PaperBundle.Origin.ElectedCandidate(Apple, transferValue), candidateStatuses)
      }

    val actualVoteCounts = VoteCounting.countVotes[Fruit](
      initialNumPapers = testPreferenceTree.numPapers,
      quota = 2l,
      candidateStatuses = candidateStatuses,
      paperBundles = paperBundles,
    )

    val expectedVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(numPapers = 0, numVotes = 2),
        Banana -> VoteCount(numPapers = 2, numVotes = 1),
        Pear -> VoteCount(numPapers = 1, numVotes = 0),
        Strawberry -> VoteCount(numPapers = 2, numVotes = 1),
      ),
      exhausted = VoteCount.zero,
      roundingError = VoteCount(numPapers = 0, numVotes = -1),
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

  it should "correctly count the votes when some ballots have exhausted" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Elected(ordinalElected = 0, electedAtCount = 1),
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Excluded(ordinalExcluded = 0, excludedAtCount = 2),
      Strawberry -> CandidateStatus.Excluded(ordinalExcluded = 1, excludedAtCount = 3),
    )

    val paperBundles: Bag[PaperBundle[Fruit]] = Bag(
      AssignedPaperBundle(
        transferValue = 0.666666666d,
        preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear, Banana).get,
        origin = PaperBundle.Origin.ElectedCandidate(Apple, 0.666666666d),
      ),
      AssignedPaperBundle(
        transferValue = 0.666666666d,
        preferenceTreeNode = testPreferenceTree.childFor(Apple, Banana).get,
        origin = PaperBundle.Origin.ElectedCandidate(Apple, 0.666666666d),
      ),
      AssignedPaperBundle(
        transferValue = 1d,
        preferenceTreeNode = testPreferenceTree.childFor(Banana).get,
        origin = PaperBundle.Origin.InitialAllocation,
      ),
      ExhaustedPaperBundle(
        numPapers = 1l,
        transferValue = 1d,
        origin = PaperBundle.Origin.ExcludedCandidate(Strawberry),
      ),
      ExhaustedPaperBundle(
        numPapers = 1l,
        transferValue = 0.666666666d,
        origin = PaperBundle.Origin.ElectedCandidate(Apple, transferValue = 0.666666666d)
      )
    )

    val actualVoteCounts = VoteCounting.countVotes[Fruit](
      initialNumPapers = testPreferenceTree.numPapers,
      quota = 2l,
      candidateStatuses = candidateStatuses,
      paperBundles = paperBundles,
    )

    val expectedVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(numPapers = 0, numVotes = 2),
        Banana -> VoteCount(numPapers = 3, numVotes = 2),
        Pear -> VoteCount(numPapers = 0, numVotes = 0),
        Strawberry -> VoteCount(numPapers = 0, numVotes = 0),
      ),
      exhausted = VoteCount(numPapers = 2, numVotes = 1),
      roundingError = VoteCount(numPapers = 0, numVotes = 0),
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }
}
