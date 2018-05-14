package au.id.tmm.countstv.counting.countsteps.distribution

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.{AssignedPaperBundle, PaperBundle}
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.DistributionCountStep
import au.id.tmm.countstv.model.values._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class DistributingPapersSpec extends ImprovedFlatSpec {

  import DistributingPapersFixture.WithFinalElection._

  "count step 2, when Watermelon is excluded" should "have the correct number of formal papers" in {
    val actualContext = actualContextAfterCount(Count(2))

    assert(actualContext.numFormalPapers === rootBundle.numPapers)
  }

  it should "have the correct number of vacancies" in {
    val actualContext = actualContextAfterCount(Count(2))

    assert(actualContext.numVacancies === numVacancies)
  }

  it should "have the correct paper bundles" in {
    val actualContext = actualContextAfterCount(Count(2))

    val expectedPaperBundles = Set[PaperBundle[Fruit]](
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Apple).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Pear).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Pear).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon, Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Raspberry).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Mango).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Apple).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon, Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Banana).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Mango).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon, Count(2)),
      ),
    )

    assert(actualContext.paperBundles === expectedPaperBundles)
  }

  it should "have produced the correct distribution step" in {
    val actualContext = actualContextAfterCount(Count(2))

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(2),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Remaining,
        Watermelon -> Excluded(Ordinal.first, Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts(
        perCandidate = Map(
          Apple -> VoteCount(11),
          Banana -> VoteCount(6),
          Mango -> VoteCount(10),
          Pear -> VoteCount(11),
          Raspberry -> VoteCount(7),
          Strawberry -> VoteCount(5),
          Watermelon -> VoteCount(0),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount.zero,
      ),
      distributionSource = DistributionCountStep.Source(
        Watermelon,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(0)),
        transferValue = TransferValue(1),
      )
    )

    assert(actualContext.mostRecentCountStep === expectedCountStep)
  }

  "count step 3, when Strawberry is excluded" should "have the correct paper bundles" in {
    val actualContext = actualContextAfterCount(Count(3))

    val expectedPaperBundles = Set[PaperBundle[Fruit]](
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Pear).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Apple).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry, Raspberry).get,
        PaperBundle.Origin.ExcludedCandidate(Strawberry,Count(3)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Raspberry).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Apple).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon,Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Banana).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Mango).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry, Apple).get,
        PaperBundle.Origin.ExcludedCandidate(Strawberry,Count(3)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry, Watermelon, Raspberry).get,
        PaperBundle.Origin.ExcludedCandidate(Strawberry,Count(3)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Mango).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon,Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Pear).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon,Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry, Banana).get,
        PaperBundle.Origin.ExcludedCandidate(Strawberry,Count(3)),
      ),
    )

    assert(actualContext.paperBundles === expectedPaperBundles)
  }

  it should "have produced the correct count step" in {
    val actualCountStep = getActualCountStep(Count(3))

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(3),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Excluded(Ordinal.second,Count(3)),
        Watermelon -> Excluded(Ordinal.first,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(12),
          Banana -> VoteCount(8),
          Mango -> VoteCount(10),
          Pear -> VoteCount(11),
          Raspberry -> VoteCount(9),
          Strawberry -> VoteCount(0),
          Watermelon -> VoteCount(0),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount.zero,
      ),
      distributionSource = DistributionCountStep.Source(
        Strawberry,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(0)),
        transferValue = TransferValue(1),
      ),
    )

    assert(actualCountStep === expectedCountStep)
  }

  "count 4, where Apple is elected" should "have produced the correct count step" in {
    val actualCountStep = getActualCountStep(Count(4))

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(4),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(4)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Excluded(Ordinal.second,Count(3)),
        Watermelon -> Excluded(Ordinal.first,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(18), NumVotes(18)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0)),
          Mango -> VoteCount(NumPapers(11), NumVotes(11)),
          Pear -> VoteCount(NumPapers(11), NumVotes(11)),
          Raspberry -> VoteCount(NumPapers(10), NumVotes(10)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(0))
      ),
      distributionSource = DistributionCountStep.Source(
        Banana,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(0), Count(3)),
        transferValue = TransferValue(1),
      ),
    )

    assert(actualCountStep === expectedCountStep)
  }

  "count 5, where Apple is being distributed" should "have produced the correct count step" in {
    val actualCountStep = getActualCountStep(Count(5))

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(5),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(4)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Excluded(Ordinal.second,Count(3)),
        Watermelon -> Excluded(Ordinal.first,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(0), NumVotes(17)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0)),
          Mango -> VoteCount(NumPapers(16), NumVotes(11)),
          Pear -> VoteCount(NumPapers(17), NumVotes(11)),
          Raspberry -> VoteCount(NumPapers(17), NumVotes(10)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(-1))
      ),
      distributionSource = DistributionCountStep.Source(
        Apple,
        CandidateDistributionReason.Election,
        sourceCounts = Set(Count(0), Count(2), Count(3), Count(4)),
        transferValue = TransferValue(1d / 18d),
      ),
    )

    assert(actualCountStep === expectedCountStep)
  }

  "count 6, where Raspberry is excluded" should "have produced the correct count step" in {
    val actualCountStep = getActualCountStep(Count(6))

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(6),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(4)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Excluded(Ordinal.fourth,Count(6)),
        Strawberry -> Excluded(Ordinal.second,Count(3)),
        Watermelon -> Excluded(Ordinal.first,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(0), NumVotes(17)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0)),
          Mango -> VoteCount(NumPapers(21), NumVotes(16)),
          Pear -> VoteCount(NumPapers(22), NumVotes(16)),
          Raspberry -> VoteCount(NumPapers(7), NumVotes(0)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(-1))
      ),
      distributionSource = DistributionCountStep.Source(
        Raspberry,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(0), Count(3), Count(4)),
        transferValue = TransferValue(1),
      ),
    )

    assert(actualCountStep === expectedCountStep)
  }

  "count 7, where Raspberry's ballots are still being distributed" should "have produced the correct count step" in {
    val actualCountStep = getActualCountStep(Count(7))

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(7),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(4)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Excluded(Ordinal.fourth,Count(6)),
        Strawberry -> Excluded(Ordinal.second,Count(3)),
        Watermelon -> Excluded(Ordinal.first,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(0), NumVotes(17)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0)),
          Mango -> VoteCount(NumPapers(25), NumVotes(16)),
          Pear -> VoteCount(NumPapers(25), NumVotes(16)),
          Raspberry -> VoteCount(NumPapers(0), NumVotes(0)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(-1))
      ),
      distributionSource = DistributionCountStep.Source(
        Raspberry,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(5)),
        transferValue = TransferValue(1d / 18d),
      ),
    )

    assert(actualCountStep === expectedCountStep)
  }
}