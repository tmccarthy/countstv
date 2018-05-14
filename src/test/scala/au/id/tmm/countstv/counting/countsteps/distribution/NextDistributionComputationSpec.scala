package au.id.tmm.countstv.counting.countsteps.distribution

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.countsteps.distribution.NextDistributionComputation.DistributionTarget
import au.id.tmm.countstv.model.CandidateDistributionReason.{Election, Exclusion}
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.{AllocationAfterIneligibles, CountSteps}
import au.id.tmm.countstv.model.values._
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, VoteCount}
import au.id.tmm.utilities.probabilities.ProbabilityMeasure
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class NextDistributionComputationSpec extends ImprovedFlatSpec {

  import au.id.tmm.countstv.counting.countsteps.CountContextFixture._

  "the next distribution target" should
    "be computed correctly when there are more than one elected candidates waiting for distribution" in {
    val mostRecentCountStep = testContext
      .mostRecentCountStep
      .asInstanceOf[AllocationAfterIneligibles[Fruit]]
      .copy(
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Elected(Ordinal.first, Count(1)),
          Banana -> Elected(Ordinal.second, Count(1)),
          Pear -> Remaining,
          Strawberry -> Remaining,
        ),
      )

    val localTestContext = testContext.copy(
      previousCountSteps = CountSteps.AfterIneligibleHandling(initialAllocation, mostRecentCountStep)
    )

    val actualDistributionTarget = NextDistributionComputation.nextCandidateToDistribute(localTestContext)
    val expectedDistributionTarget = DistributionTarget(Apple, Election, TransferValueCoefficient(0.3))

    assert(actualDistributionTarget === ProbabilityMeasure.always(expectedDistributionTarget))
  }

  it should "be computed correctly when there are no elected candidates waiting for distribution" in {
    val mostRecentCountStep = testContext
      .mostRecentCountStep
      .asInstanceOf[AllocationAfterIneligibles[Fruit]]
      .copy(
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Elected(Ordinal.first, Count(1)),
          Banana -> Remaining,
          Pear -> Remaining,
          Strawberry -> Remaining,
        ),
        candidateVoteCounts = CandidateVoteCounts[Fruit](
          perCandidate = Map[Fruit, VoteCount](
            Apple -> VoteCount(NumPapers(0), NumVotes(14)),
            Banana -> VoteCount(NumPapers(12), NumVotes(10)),
            Pear -> VoteCount(NumPapers(16), NumVotes(6)),
            Strawberry -> VoteCount(NumPapers(12), NumVotes(10)),
          ),
          exhausted = VoteCount.zero,
          roundingError = VoteCount.zero,
        )
      )

    val localTestContext = testContext.copy(
      previousCountSteps = CountSteps.AfterIneligibleHandling(initialAllocation, mostRecentCountStep)
    )

    val actualDistributionTarget = NextDistributionComputation.nextCandidateToDistribute(localTestContext)
    val expectedDistributionTarget = DistributionTarget(Pear, Exclusion, TransferValueCoefficient(1.0))

    assert(actualDistributionTarget === ProbabilityMeasure.always(expectedDistributionTarget))
  }

}
