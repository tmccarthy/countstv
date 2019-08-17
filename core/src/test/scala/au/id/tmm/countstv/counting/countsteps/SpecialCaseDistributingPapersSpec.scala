package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.fixtures.CountStepFixtures
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.{ElectedNoSurplusCountStep, ExcludedNoVotesCountStep}
import au.id.tmm.countstv.model.values.{Count, NumPapers, NumVotes, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, VoteCount}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class SpecialCaseDistributingPapersSpec extends ImprovedFlatSpec {

  "an election where a candidate has zero votes" should "exclude that candidate in a step without distributing anything" in {

    val expectedCountStep = ExcludedNoVotesCountStep(
      Count(2),
      candidateStatuses = CandidateStatuses(
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Excluded(Ordinal.second, Count(2)),
        Watermelon -> Excluded(Ordinal.first, Count(1)),
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
      excludedCandidate = Watermelon,
    )

    val actualCountStep = CountStepFixtures.DuringDistributions.whereCandidateExcludedWithoutVotes

    assert(actualCountStep === expectedCountStep)

  }

  "an election where a candidate is elected with no surplus" should "elect that candidate in a step without distributing anything" in {
    val expectedCountStep = ElectedNoSurplusCountStep(
      count = Count(5),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(3)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Excluded(Ordinal.fourth, Count(5)),
        Strawberry -> Excluded(Ordinal.second,Count(2)),
        Watermelon -> Excluded(Ordinal.first,Count(1)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(0), NumVotes(17)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0)),
          Mango -> VoteCount(NumPapers(11), NumVotes(11)),
          Pear -> VoteCount(NumPapers(11), NumVotes(11)),
          Raspberry -> VoteCount(NumPapers(11), NumVotes(11)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(17), NumVotes(0)) // TODO the missing papers aren't really a rounding error
      ),
      electedCandidate = Apple,
      sourceCounts = Set(Count(0), Count(3), Count(4)),
    )

    val actualCountStep = CountStepFixtures.DuringDistributions.whereCandidateElectedWithoutSurplus

    assert(actualCountStep === expectedCountStep)
  }
}
