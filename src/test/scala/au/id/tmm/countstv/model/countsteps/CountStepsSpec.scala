package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.values._
import au.id.tmm.countstv.model._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CountStepsSpec extends ImprovedFlatSpec {

  private val testInitialAllocation = InitialAllocation(
    candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Remaining,
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Ineligible,
      Strawberry -> CandidateStatus.Ineligible,
    ),
    candidateVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(NumPapers(32), NumVotes(42)),
        Banana -> VoteCount(NumPapers(32), NumVotes(42)),
        Pear -> VoteCount(NumPapers(32), NumVotes(42)),
        Strawberry -> VoteCount(NumPapers(32), NumVotes(42)),
      ),
      exhausted = VoteCount(NumPapers(0), NumVotes(0)),
      roundingError = VoteCount(NumPapers(0), NumVotes(0)),
    )
  )

  private val testAllocationAfterIneligibles = AllocationAfterIneligibles(
    candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Remaining,
      Banana -> Remaining,
      Pear -> Remaining,
      Strawberry -> Remaining,
    ),
    candidateVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map[Fruit, VoteCount](
        Apple -> VoteCount(40),
        Banana -> VoteCount(30),
        Pear -> VoteCount(20),
        Strawberry -> VoteCount(10),
      ),
      exhausted = VoteCount.zero,
      roundingError = VoteCount.zero,
    ),
    transfersDueToIneligibles = Map.empty[Fruit, CandidateVoteCounts[Fruit]],
  )

  private val testDistributionCountStep = DistributionCountStep[Fruit](
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
    distributionSource = Some(DistributionCountStep.Source(
      Watermelon,
      CandidateDistributionReason.Exclusion,
      sourceCounts = Set(Count(0)),
      transferValue = TransferValue(1),
    ))
  )

  "a collection of count steps" should "always contain an initial step" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = None,
      distributionCountSteps = Nil,
    )

    assert(countSteps.initialAllocation === testInitialAllocation)
  }

  it can "be missing an allocation away from ineligible candidates" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = None,
      distributionCountSteps = Nil,
    )

    assert(countSteps.allocationAfterIneligibles === None)
  }

  it can "have an allocation away from from ineligible candidates" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = Some(testAllocationAfterIneligibles),
      distributionCountSteps = Nil,
    )

    assert(countSteps.allocationAfterIneligibles === Some(testAllocationAfterIneligibles))
  }

  it should "have a list of distribution steps" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = Some(testAllocationAfterIneligibles),
      distributionCountSteps = List(testDistributionCountStep),
    )

    assert(countSteps.distributionCountSteps === List(testDistributionCountStep))
  }

  it can "be represented as a list when there was only one step" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = None,
      distributionCountSteps = Nil,
    )

    assert(countSteps.asList === List(testInitialAllocation))
  }

  it can "be represented as a list when there was an allocation away from ineligibles and no distributions" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = Some(testAllocationAfterIneligibles),
      distributionCountSteps = Nil,
    )

    assert(countSteps.asList === List(testInitialAllocation, testAllocationAfterIneligibles))
  }

  it can "be represented as a list when there was no allocation away from ineligibles and a distribution" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = None,
      distributionCountSteps = List(testDistributionCountStep),
    )

    assert(countSteps.asList === List(testInitialAllocation, testDistributionCountStep))
  }

  it can "be represented as a list when there was an allocation away from ineligibles and a distribution" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = Some(testAllocationAfterIneligibles),
      distributionCountSteps = List(testDistributionCountStep),
    )

    assert(countSteps.asList === List(testInitialAllocation, testAllocationAfterIneligibles, testDistributionCountStep))
  }

  it should "expose the last count step when there was only one step" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = None,
      distributionCountSteps = Nil,
    )

    assert(countSteps.last === testInitialAllocation)
  }

  it should "expose the last count step when there was an allocation away from ineligibles and no distributions" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = Some(testAllocationAfterIneligibles),
      distributionCountSteps = Nil,
    )

    assert(countSteps.last === testAllocationAfterIneligibles)
  }

  it should "expose the last count step when there was no allocation away from ineligibles and a distribution" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = None,
      distributionCountSteps = List(testDistributionCountStep),
    )

    assert(countSteps.last === testDistributionCountStep)
  }

  it should "expose the last count step when there was an allocation away from ineligibles and a distribution" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = Some(testAllocationAfterIneligibles),
      distributionCountSteps = List(testDistributionCountStep),
    )

    assert(countSteps.last === testDistributionCountStep)
  }

  it can "have a distribution step appended" in {
    val countSteps = CountSteps[Fruit](
      initialAllocation = testInitialAllocation,
      allocationAfterIneligibles = Some(testAllocationAfterIneligibles),
      distributionCountSteps = Nil,
    )

    val withAppendedDistributionStep = countSteps.append(testDistributionCountStep)

    assert(withAppendedDistributionStep.distributionCountSteps === List(testDistributionCountStep))
  }
}
