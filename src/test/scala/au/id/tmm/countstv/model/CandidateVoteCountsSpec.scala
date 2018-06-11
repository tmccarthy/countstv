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
    roundingError = VoteCount(NumPapers(0), NumVotes(13)),
  )

  "a collection of candidate vote counts" should "store the number of exhausted votes" in {
    assert(testCandidateVoteCounts.exhausted === VoteCount(42))
  }

  it should "store the votes gained or lost due to rounding" in {
    assert(testCandidateVoteCounts.roundingError === VoteCount(NumPapers(0), NumVotes(13)))
  }

  it should "store the votes per candidate" in {
    assert(testCandidateVoteCounts.perCandidate === Map(
      Apple -> VoteCount(3),
      Banana -> VoteCount(2),
      Pear -> VoteCount(1),
      Strawberry -> VoteCount(0),
    ))
  }

  it can "have a diff with another instance" in {
    val newCandidateVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(7),
        Banana -> VoteCount(1),
        Pear -> VoteCount(5),
        Strawberry -> VoteCount(2),
      ),
      exhausted = VoteCount(70),
      roundingError = VoteCount(NumPapers(0), NumVotes(2)),
    )

    val expectedDiff = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(4),
        Banana -> VoteCount(-1),
        Pear -> VoteCount(4),
        Strawberry -> VoteCount(2),
      ),
      exhausted = VoteCount(28),
      roundingError = VoteCount(NumPapers(0), NumVotes(-11)),
    )

    assert((newCandidateVoteCounts diff testCandidateVoteCounts) === expectedDiff)
  }

  it can "have its total vote count computed" in {
    assert(testCandidateVoteCounts.total === VoteCount(NumPapers(48), NumVotes(61)))
  }

  it can "have its total vote count computed if there are no votes for candidates" in {
    val testCandidateVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map.empty,
      exhausted = VoteCount(2),
      roundingError = VoteCount(3),
    )
    assert(testCandidateVoteCounts.total === VoteCount(5))
  }
}
