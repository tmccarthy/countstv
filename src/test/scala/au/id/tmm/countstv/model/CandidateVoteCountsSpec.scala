package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CandidateVoteCountsSpec extends ImprovedFlatSpec {

  private val testCandidateVoteCounts = CandidateVoteCounts[Fruit](
    perCandidate = Map(
      Apple -> VoteCount(3, 3d),
      Banana -> VoteCount(2, 2d),
      Pear -> VoteCount(1, 1d),
      Strawberry -> VoteCount(0, 0d),
    ),
    exhausted = VoteCount(42, 42d),
    roundingError = VoteCount(0, 13.4d),
  )

  "a collection of candidate vote counts" should "store the number of exhausted votes" in {
    assert(testCandidateVoteCounts.exhausted === VoteCount(42, 42d))
  }

  it should "store the votes gained or lost due to rounding" in {
    assert(testCandidateVoteCounts.roundingError === VoteCount(0, 13.4d))
  }

  it should "store the votes per candidate" in {
    assert(testCandidateVoteCounts.perCandidate === Map(
      Apple -> VoteCount(3, 3d),
      Banana -> VoteCount(2, 2d),
      Pear -> VoteCount(1, 1d),
      Strawberry -> VoteCount(0, 0d),
    ))
  }
}
