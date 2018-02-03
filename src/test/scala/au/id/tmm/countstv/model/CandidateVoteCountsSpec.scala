package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CandidateVoteCountsSpec extends ImprovedFlatSpec {

  private val testCandidateVoteCounts = CandidateVoteCounts[Fruit](
    perCandidate = Map(
      Apple -> VoteCount(3),
      Banana -> VoteCount(2),
      Pear -> VoteCount(1),
      Strawberry -> VoteCount(0),
    ),
    exhausted = VoteCount(42),
    roundingError = VoteCount(NumPapers(0), NumVotes(13.4d)),
  )

  "a collection of candidate vote counts" should "store the number of exhausted votes" in {
    assert(testCandidateVoteCounts.exhausted === VoteCount(42))
  }

  it should "store the votes gained or lost due to rounding" in {
    assert(testCandidateVoteCounts.roundingError === VoteCount(NumPapers(0), NumVotes(13.4d)))
  }

  it should "store the votes per candidate" in {
    assert(testCandidateVoteCounts.perCandidate === Map(
      Apple -> VoteCount(3),
      Banana -> VoteCount(2),
      Pear -> VoteCount(1),
      Strawberry -> VoteCount(0),
    ))
  }
}
