package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.values._
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

  private val secondDistributionStep = testDistributionCountStep.copy(count = testDistributionCountStep.count.increment)

  private val fullTestCountSteps = CountSteps[Fruit](
    initialAllocation = testInitialAllocation,
    allocationAfterIneligibles = Some(testAllocationAfterIneligibles),
    distributionCountSteps = List(testDistributionCountStep),
  )

  "a collection of count steps" should "always contain an initial step" in {
    val countSteps = fullTestCountSteps.copy(allocationAfterIneligibles = None, distributionCountSteps = Nil)

    assert(countSteps.initialAllocation === testInitialAllocation)
  }

  it can "be missing an allocation away from ineligible candidates" in {
    val countSteps = fullTestCountSteps.copy(allocationAfterIneligibles = None, distributionCountSteps = Nil)

    assert(countSteps.allocationAfterIneligibles === None)
  }

  it can "have an allocation away from from ineligible candidates" in {
    val countSteps = fullTestCountSteps.copy(distributionCountSteps = Nil)

    assert(countSteps.allocationAfterIneligibles === Some(testAllocationAfterIneligibles))
  }

  it can "not be missing an allocation away from ineligibles and also have a distribution" in {
    intercept[IllegalArgumentException] {
      fullTestCountSteps.copy(allocationAfterIneligibles = None)
    }
  }

  it should "have a list of distribution steps" in {
    assert(fullTestCountSteps.distributionCountSteps === List(testDistributionCountStep))
  }

  it can "be represented as a list when there was only one step" in {
    val countSteps = fullTestCountSteps.copy(allocationAfterIneligibles = None, distributionCountSteps = Nil)

    assert(countSteps.toList === List(testInitialAllocation))
  }

  it can "be represented as a list when there was an allocation away from ineligibles and no distributions" in {
    val countSteps = fullTestCountSteps.copy(distributionCountSteps = Nil)

    assert(countSteps.toList === List(testInitialAllocation, testAllocationAfterIneligibles))
  }

  it can "be represented as a list when there was an allocation away from ineligibles and a distribution" in {
    assert(fullTestCountSteps.toList === List(testInitialAllocation, testAllocationAfterIneligibles, testDistributionCountStep))
  }

  it should "expose the last count step when there was only one step" in {
    val countSteps = fullTestCountSteps.copy(allocationAfterIneligibles = None, distributionCountSteps = Nil)

    assert(countSteps.last === testInitialAllocation)
  }

  it should "expose the last count step when there was an allocation away from ineligibles and no distributions" in {
    val countSteps = fullTestCountSteps.copy(distributionCountSteps = Nil)

    assert(countSteps.last === testAllocationAfterIneligibles)
  }

  it should "expose the last count step when there was an allocation away from ineligibles and a distribution" in {
    assert(fullTestCountSteps.last === testDistributionCountStep)
  }

  it should "return the initial allocation as the head" in {
    assert(fullTestCountSteps.head === testInitialAllocation)
  }

  it can "have a distribution step appended" in {
    val countSteps = fullTestCountSteps.copy(distributionCountSteps = Nil)

    val withAppendedDistributionStep = countSteps.append(testDistributionCountStep)

    assert(withAppendedDistributionStep.distributionCountSteps === List(testDistributionCountStep))
  }

  it should "have a size" in {
    assert(fullTestCountSteps.size === 3)
  }

  it can "not be empty" in {
    assert(fullTestCountSteps.isEmpty === false)
  }

  it should "not be defined for a negative count" in {
    assert(!fullTestCountSteps.isDefinedAt(Count(-1)))
  }

  it should "be defined for the present count numbers" in {
    Range.inclusive(0, 2).foreach { countAsInt =>
      assert(fullTestCountSteps.isDefinedAt(Count(countAsInt)), countAsInt)
    }
  }

  it should "not be defined for a count greater than the last count" in {
    assert(!fullTestCountSteps.isDefinedAt(Count(3)))
  }

  it should "return the initial allocation for count 0" in {
    assert(fullTestCountSteps(Count.ofInitialAllocation) === testInitialAllocation)
  }

  it should "return the allocation after ineligibles handling for count 1" in {
    assert(fullTestCountSteps(Count.ofIneligibleCandidateHandling) === testAllocationAfterIneligibles)
  }

  it should "return the distribution step for a count greater than 1" in {
    assert(fullTestCountSteps(Count(2)) === testDistributionCountStep)
  }

  it should "throw if asked for an ineligibles handling step and one is missing" in {
    val countSteps = fullTestCountSteps.copy(allocationAfterIneligibles = None, distributionCountSteps = Nil)

    intercept[IndexOutOfBoundsException] {
      countSteps(Count.ofIneligibleCandidateHandling)
    }
  }

  it should "throw if asked for a count step beyond the bounds of the count" in {
    intercept[IndexOutOfBoundsException] {
      fullTestCountSteps(Count(5))
    }
  }

  it should "return nothing if there is no second last count step" in {
    val countSteps = fullTestCountSteps.copy(allocationAfterIneligibles = None, distributionCountSteps = Nil)

    assert(countSteps.secondLast === None)
  }

  it can "return the second last count step if it is the initial allocation" in {
    val countSteps = fullTestCountSteps.copy(distributionCountSteps = Nil)

    assert(countSteps.secondLast === Some(testInitialAllocation))
  }

  it can "return the second last count step if it is the allocation after ineligible handling" in {
    assert(fullTestCountSteps.secondLast === Some(testAllocationAfterIneligibles))
  }

  it can "return the second last count step if it is a distribution step" in {
    val countSteps = fullTestCountSteps.copy(distributionCountSteps = List(testDistributionCountStep, secondDistributionStep))

    assert(countSteps.secondLast === Some(testDistributionCountStep))
  }

  it can "be truncated to the initial allocation" in {
    val actualTruncatedCountSteps = fullTestCountSteps.truncateAfter(Count.ofInitialAllocation)

    val expectedTruncatedCountSteps =
      fullTestCountSteps.copy(allocationAfterIneligibles = None, distributionCountSteps = Nil)

    assert(actualTruncatedCountSteps === expectedTruncatedCountSteps)
  }

  it can "be truncated to the allocation after ineligible handling" in {
    val actualTruncatedCountSteps = fullTestCountSteps.truncateAfter(Count.ofIneligibleCandidateHandling)

    val expectedTruncatedCountSteps = fullTestCountSteps.copy(distributionCountSteps = Nil)

    assert(actualTruncatedCountSteps === expectedTruncatedCountSteps)
  }

  it can "be truncated to a distribution step" in {
    val initialCountSteps = fullTestCountSteps.copy(distributionCountSteps = List(testDistributionCountStep, secondDistributionStep))

    val actualTruncatedCountSteps = initialCountSteps.truncateAfter(Count(2))

    val expectedTruncatedCountSteps = fullTestCountSteps

    assert(actualTruncatedCountSteps === expectedTruncatedCountSteps)
  }
}
