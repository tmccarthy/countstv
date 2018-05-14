package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.FinalElectionCountStep
import au.id.tmm.countstv.model.values.{Count, NumPapers, NumVotes, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, VoteCount}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class FinalElectionComputationSpec extends ImprovedFlatSpec {

  import distribution.DistributingPapersFixture.WithFinalElection._

  "the count after step 5, after apple has been distributed" should "not return a final election" in {
    assert(FinalElectionComputation.contextAfterFinalElection(actualContextAfterCount(Count(5))) === None)
  }

  "count 8, where Pear is elected to the remaining vacancy" should "have produced the correct count step" in {
    val contextAfterStep7 = actualContextAfterCount(Count(7))

    val actualContext = FinalElectionComputation.contextAfterFinalElection(contextAfterStep7)
    val actualCountStep = actualContext.map(_.onlyOutcome.previousCountSteps.last)

    val expectedCountStep = FinalElectionCountStep[Fruit](
      count = Count(8),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(4)),
        Mango -> Remaining,
        Pear -> Elected(Ordinal.second,Count(8)),
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
    )

    assert(actualCountStep === Some(expectedCountStep))
  }

  it should "be marked as complete" in {
    val contextAfterStep7 = actualContextAfterCount(Count(7))

    val actualContext = FinalElectionComputation.contextAfterFinalElection(contextAfterStep7)

    assert(actualContext.map(_.onlyOutcome.allVacanciesNowFilled) === Some(true))
  }

}
