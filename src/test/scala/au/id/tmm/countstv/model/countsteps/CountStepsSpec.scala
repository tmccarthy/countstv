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
    distributionSource = DistributionCountStep.Source(
      Watermelon,
      CandidateDistributionReason.Exclusion,
      sourceCounts = Set(Count(0)),
      transferValue = TransferValue(1),
    ),
  )

  private val secondDistributionStep = testDistributionCountStep.copy(count = testDistributionCountStep.count.increment)

  private val testFinalElectionStep = FinalElectionCountStep[Fruit](
    count = Count(3),
    candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Elected(Ordinal.first, Count(3)),
      Banana -> Elected(Ordinal.second, Count(3)),
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
  )

  private val testCountStepsInitial: CountSteps.Initial[Fruit] = CountSteps.Initial(testInitialAllocation)
  private val testCountStepsAfterIneligibleHandling: CountSteps.AfterIneligibleHandling[Fruit] = CountSteps.AfterIneligibleHandling(testInitialAllocation, testAllocationAfterIneligibles)
  private val testCountStepsDuringDistributions: CountSteps.DuringDistributions[Fruit] = CountSteps.DuringDistributions(testInitialAllocation, testAllocationAfterIneligibles, List(testDistributionCountStep))
  private val testCountStepsAfterFinalElections: CountSteps.AfterFinalElections[Fruit] = CountSteps.AfterFinalElections(testInitialAllocation, testAllocationAfterIneligibles, List(testDistributionCountStep), testFinalElectionStep)

  behaviour of "an initial CountSteps instance"

  standardTests(testCountStepsInitial)(
    expectedHead = testInitialAllocation,
    expectedLast = testInitialAllocation,
    expectedAsList = List(testInitialAllocation),
    expectedSize = 1,
    expectedDefinedUpToCount = Count.ofInitialAllocation,
    expectedWhenTruncatedAtCount = List(
      Count(0) -> testCountStepsInitial,
      Count(42) -> testCountStepsInitial,
    ),
  )

  it should "contain an initial step" in {
    assert(testCountStepsInitial.initialAllocation === testInitialAllocation)
  }

  it can "have an ineligibles-handling step appended" in {
    assert(testCountStepsInitial.append(testAllocationAfterIneligibles) === testCountStepsAfterIneligibleHandling)
  }

  behaviour of "a CountSteps instance after ineligible handling"

  standardTests(testCountStepsAfterIneligibleHandling)(
    expectedHead = testInitialAllocation,
    expectedLast = testAllocationAfterIneligibles,
    expectedAsList = List(testInitialAllocation, testAllocationAfterIneligibles),
    expectedSize = 2,
    expectedDefinedUpToCount = Count.ofIneligibleCandidateHandling,
    expectedWhenTruncatedAtCount = List(
      Count(0) -> testCountStepsInitial,
      Count(1) -> testCountStepsAfterIneligibleHandling,
      Count(42) -> testCountStepsAfterIneligibleHandling,
    )
  )

  it should "contain an initial step" in {
    assert(testCountStepsAfterIneligibleHandling.initialAllocation === testInitialAllocation)
  }

  it should "contain an allocation after ineligible candidates" in {
    assert(testCountStepsAfterIneligibleHandling.allocationAfterIneligibles === testAllocationAfterIneligibles)
  }

  it can "have a distribution step appended" in {
    val countSteps = testCountStepsAfterIneligibleHandling

    val withAppendedDistributionStep = countSteps.append(testDistributionCountStep)

    assert(withAppendedDistributionStep === CountSteps.DuringDistributions(testInitialAllocation, testAllocationAfterIneligibles, List(testDistributionCountStep)))
  }

  it can "have a final election step appended" in {
    val countSteps = testCountStepsAfterIneligibleHandling

    val withAppendedDistributionStep = countSteps.append(testFinalElectionStep)

    assert(withAppendedDistributionStep === CountSteps.AfterFinalElections(testInitialAllocation, testAllocationAfterIneligibles, Nil, testFinalElectionStep))
  }

  behaviour of "a CountSteps instance during distribution steps"

  standardTests(testCountStepsDuringDistributions)(
    expectedHead = testInitialAllocation,
    expectedLast = testDistributionCountStep,
    expectedAsList = List(testInitialAllocation, testAllocationAfterIneligibles, testDistributionCountStep),
    expectedSize = 3,
    expectedDefinedUpToCount = Count(2),
    expectedWhenTruncatedAtCount = List(
      Count(0) -> testCountStepsInitial,
      Count(1) -> testCountStepsAfterIneligibleHandling,
      Count(2) -> testCountStepsDuringDistributions,
      Count(42) -> testCountStepsDuringDistributions,
    )
  )

  it should "contain an initial step" in {
    assert(testCountStepsDuringDistributions.initialAllocation === testInitialAllocation)
  }

  it should "contain an allocation after ineligible candidates" in {
    assert(testCountStepsDuringDistributions.allocationAfterIneligibles === testAllocationAfterIneligibles)
  }

  it should "contain a list of distribution steps" in {
    assert(testCountStepsDuringDistributions.distributionCountSteps === List(testDistributionCountStep))
  }

  it can "have a distribution step appended" in {
    val countSteps = testCountStepsDuringDistributions

    val withAppendedDistributionStep = countSteps.append(secondDistributionStep)

    assert(withAppendedDistributionStep.distributionCountSteps === List(testDistributionCountStep, secondDistributionStep))
  }

  it can "have a final election step appended" in {
    val countSteps = testCountStepsDuringDistributions

    val withAppendedDistributionStep = countSteps.append(testFinalElectionStep)

    assert(withAppendedDistributionStep === testCountStepsAfterFinalElections)
  }

  it can "be truncated to a distribution step" in {
    val initialCountSteps = testCountStepsDuringDistributions.append(secondDistributionStep)

    val actualTruncatedCountSteps = initialCountSteps.truncateAfter(Count(2))

    val expectedTruncatedCountSteps = testCountStepsDuringDistributions

    assert(actualTruncatedCountSteps === expectedTruncatedCountSteps)
  }

  it should "throw if constructed with an empty list of distribution steps" in {
    intercept[IllegalArgumentException] {
      CountSteps.DuringDistributions(testInitialAllocation, testAllocationAfterIneligibles, Nil)
    }
  }

  behaviour of "a CountSteps instance after a final election step"

  standardTests(testCountStepsAfterFinalElections)(
    expectedHead = testInitialAllocation,
    expectedLast = testFinalElectionStep,
    expectedAsList = List(testInitialAllocation, testAllocationAfterIneligibles, testDistributionCountStep, testFinalElectionStep),
    expectedSize = 4,
    expectedDefinedUpToCount = Count(3),
    expectedWhenTruncatedAtCount = List(
      Count(0) -> testCountStepsInitial,
      Count(1) -> testCountStepsAfterIneligibleHandling,
      Count(2) -> testCountStepsDuringDistributions,
      Count(3) -> testCountStepsAfterFinalElections,
      Count(42) -> testCountStepsAfterFinalElections,
    )
  )

  it should "contain an initial step" in {
    assert(testCountStepsAfterFinalElections.initialAllocation === testInitialAllocation)
  }

  it should "contain an allocation after ineligible candidates" in {
    assert(testCountStepsAfterFinalElections.allocationAfterIneligibles === testAllocationAfterIneligibles)
  }

  it should "contain a list of distribution steps" in {
    assert(testCountStepsAfterFinalElections.distributionCountSteps === List(testDistributionCountStep))
  }

  it should "contain a final election step" in {
    assert(testCountStepsAfterFinalElections.finalElectionCountStep === testFinalElectionStep)
  }

  private def standardTests(testInstance: CountSteps[Fruit])
                           (
                             expectedHead: InitialAllocation[Fruit],
                             expectedLast: CountStep[Fruit],
                             expectedAsList: List[CountStep[Fruit]],
                             expectedSize: Int,
                             expectedDefinedUpToCount: Count,
                             expectedWhenTruncatedAtCount: List[(Count, CountSteps[Fruit])],
                           ): Unit = {
    it should "have the initial allocation as the head" in {
      assert(testInstance.head === expectedHead)
    }

    it should "expose the last step" in {
      assert(testInstance.last === expectedLast)
    }

    it can "be converted to a list of count steps" in {
      assert(testInstance.toList === expectedAsList)
    }

    it should s"have the correct size" in {
      assert(testInstance.size === expectedSize)
    }

    it should "have a definite size" in {
      assert(testInstance.hasDefiniteSize === true)
    }

    it should "not be empty" in {
      assert(testInstance.isEmpty === false)
    }

    for (countAsInt <- 0 to expectedDefinedUpToCount.asInt) {
      it should s"be defined for count $countAsInt" in {
        assert(testInstance.isDefinedAt(Count(countAsInt)))
      }

      it should s"return the correct count for count $countAsInt" in {
        assert(testInstance(Count(countAsInt)) === expectedAsList(countAsInt))
      }
    }

    for (countAsInt <- (expectedDefinedUpToCount.asInt + 1) to (expectedDefinedUpToCount.asInt + 3)) {
      it should s"not be defined for count $countAsInt" in {
        assert(!testInstance.isDefinedAt(Count(countAsInt)))
      }

      it should s"throw when attempting to extract a count for count $countAsInt" in {
        intercept[IndexOutOfBoundsException] {
          testInstance(Count(countAsInt))
        }
      }
    }

    for ((count, expectedTruncated) <- expectedWhenTruncatedAtCount) {
      it should s"correctly truncate to count $count" in {
        assert(testInstance.truncateAfter(count) === expectedTruncated)
      }
    }
  }

}
