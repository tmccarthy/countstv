package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit.{Mango, _}
import au.id.tmm.countstv.counting.fixtures.CountStepFixtures
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.FinalElectionCountStep
import au.id.tmm.countstv.model.values.{Count, NumPapers, NumVotes, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, VoteCount}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class FinalElectionComputationSpec extends ImprovedFlatSpec {

  "count 8, where Pear is elected to the remaining vacancy" should "have produced the correct count step" in {
    val actualCountStep = CountStepFixtures.AfterFinalStep.whereCandidateElectedToRemainingVacancy

    val expectedCountStep = FinalElectionCountStep[Fruit](
      count = Count(8),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(3)),
        Mango -> Remaining,
        Pear -> Elected(Ordinal.second,Count(8)),
        Raspberry -> Excluded(Ordinal.fourth, Count(5)),
        Strawberry -> Excluded(Ordinal.second,Count(2)),
        Watermelon -> Excluded(Ordinal.first,Count(1)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(0), NumVotes(17)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0)),
          Mango -> VoteCount(NumPapers(25), NumVotes(16)),
          Pear -> VoteCount(NumPapers(25), NumVotes(16)),
          Raspberry -> VoteCount(NumPapers(0), NumVotes(-1)), // This is a weird outcome as a result of dead-reckoned counts
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(2))
      ),
      electedCandidates = DupelessSeq(
        Pear,
      ),
    )

    assert(actualCountStep === expectedCountStep)
  }

  "a count where the number of vacancies equals the number of candidates" should "produce the correct count step" in {
    val actualCountStep = CountStepFixtures.AfterFinalStep.whereAllRemainingCandidatesMarkedElected

    val expectedCountStep = FinalElectionCountStep[Fruit](
      count = Count(2),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(2)),
        Banana -> Elected(Ordinal.fifth, Count(2)),
        Mango -> Elected(Ordinal.second, Count(2)),
        Pear -> Elected(Ordinal.third, Count(2)),
        Raspberry -> Elected(Ordinal.fourth, Count(2)),
        Strawberry -> Elected(Ordinal.sixth, Count(2)),
        Watermelon -> Elected(Ordinal.seventh, Count(2)),
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
      electedCandidates = DupelessSeq(
        Apple,
        Mango,
        Pear,
        Raspberry,
        Banana,
        Strawberry,
        Watermelon,
      ),
    )

    assert(actualCountStep === expectedCountStep)
  }
}
