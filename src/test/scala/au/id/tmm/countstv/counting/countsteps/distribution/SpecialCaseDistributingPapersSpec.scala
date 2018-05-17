package au.id.tmm.countstv.counting.countsteps.distribution

import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.ExcludedNoVotesCountStep
import au.id.tmm.countstv.model.values.{Count, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, VoteCount}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class SpecialCaseDistributingPapersSpec extends ImprovedFlatSpec {

  "an election where a candidate has zero votes" should "exclude that candidate in a step without distributing anything" in {

    import DistributingPapersFixture.WithVotelessCandidate._

    val expectedCountStep = ExcludedNoVotesCountStep(
      Count(2),
      candidateStatuses = CandidateStatuses(
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
      excludedCandidate = Watermelon,
    )

    val actualCountStep = actualContextAfterCount(Count(2)).mostRecentCountStep

    assert(actualCountStep === expectedCountStep)

  }

}
