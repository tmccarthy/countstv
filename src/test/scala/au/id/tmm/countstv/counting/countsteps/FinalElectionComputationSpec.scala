package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit.{Mango, _}
import au.id.tmm.countstv.counting.fixtures.CountStepFixtures
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.{AllocationAfterIneligibles, DistributionCountStep}
import au.id.tmm.countstv.model.values._
import au.id.tmm.countstv.model.{CandidateDistributionReason, CandidateStatuses, CandidateVoteCounts, VoteCount}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class FinalElectionComputationSpec extends ImprovedFlatSpec {

  "count 7, where Raspberry is distributed and Pear is elected" should "have produced the correct count step" in {
    val actualCountStep =
      CountStepFixtures.DuringDistributions.wherePapersWorthNoVotesAreDistributed

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(7),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(3)),
        Mango -> Remaining,
        Pear -> Elected(Ordinal.second, Count(7)),
        Raspberry -> Excluded(Ordinal.fourth, Count(5)),
        Strawberry -> Excluded(Ordinal.second,Count(2)),
        Watermelon -> Excluded(Ordinal.first,Count(1)),
      ),
      candidateVoteCounts = CandidateVoteCounts(
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(0), NumVotes(17)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0)),
          Mango -> VoteCount(NumPapers(25), NumVotes(16)),
          Pear -> VoteCount(NumPapers(25), NumVotes(16)),
          Raspberry -> VoteCount(NumPapers(0), NumVotes(0)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0)),
        ),
        exhausted = VoteCount(NumPapers(0), NumVotes(0)),
        roundingError = VoteCount(NumPapers(0), NumVotes(1))
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

  "a count where the number of vacancies equals the number of candidates" should "produce the correct count step" in {
    val actualCountStep = CountStepFixtures.AfterFinalStep.whereAllRemainingCandidatesMarkedElected

    val expectedCountStep = AllocationAfterIneligibles[Fruit](
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Elected(Ordinal.fifth, Count(1)),
        Mango -> Elected(Ordinal.second, Count(1)),
        Pear -> Elected(Ordinal.third, Count(1)),
        Raspberry -> Elected(Ordinal.fourth, Count(1)),
        Strawberry -> Elected(Ordinal.sixth, Count(1)),
        Watermelon -> Elected(Ordinal.seventh, Count(1)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(11),
          Banana -> VoteCount(6),
          Mango -> VoteCount(9),
          Pear -> VoteCount(8),
          Raspberry -> VoteCount(7),
          Strawberry -> VoteCount(5),
          Watermelon -> VoteCount(4),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(0))
      ),
      transfersDueToIneligibles = Map.empty,
    )

    assert(actualCountStep === expectedCountStep)
  }
}
