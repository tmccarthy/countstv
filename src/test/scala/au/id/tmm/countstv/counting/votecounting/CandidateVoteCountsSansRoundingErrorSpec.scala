package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.{CandidateVoteCounts, VoteCount}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CandidateVoteCountsSansRoundingErrorSpec extends ImprovedFlatSpec {

  private val testVoteCounts1 = CandidateVoteCountsSansRoundingError[Fruit](
    perCandidate = Map(
      Apple -> VoteCount(32),
      Banana -> VoteCount(1),
      Pear -> VoteCount(5),
      Strawberry -> VoteCount(6),
    ),
    exhausted = VoteCount(2),
  )


  "candidate vote counts without a rounding error" should "contain a total of all votes" in {
    assert(testVoteCounts1.total === VoteCount(46))
  }

  it can "be given a rounding error" in {
    val expectedVoteCountsWithRoundingError = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(32),
        Banana -> VoteCount(1),
        Pear -> VoteCount(5),
        Strawberry -> VoteCount(6),
      ),
      exhausted = VoteCount(2),
      roundingError = VoteCount(5),
    )

    val actualVoteCountsWithRoundingError = testVoteCounts1.withRoundingError(VoteCount(5))

    assert(actualVoteCountsWithRoundingError === expectedVoteCountsWithRoundingError)
  }

}
