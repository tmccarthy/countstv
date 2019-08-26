package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}
import au.id.tmm.countstv.model.{CandidateVoteCounts, VoteCount}
import org.scalatest.FlatSpec

class CandidateVoteCountsSansRoundingErrorSpec extends FlatSpec {

  private val testVoteCounts1 = CandidateVoteCountsSansRoundingError[Fruit](
    perCandidate = Map(
      Apple      -> VoteCount(32),
      Banana     -> VoteCount(1),
      Pear       -> VoteCount(5),
      Strawberry -> VoteCount(6),
    ),
    exhausted = VoteCount(2),
  )

  private val testVoteCounts2 = CandidateVoteCountsSansRoundingError[Fruit](
    perCandidate = Map(
      Apple      -> VoteCount(-2),
      Banana     -> VoteCount(5),
      Pear       -> VoteCount(3),
      Strawberry -> VoteCount(4),
    ),
    exhausted = VoteCount(2),
  )

  "candidate vote counts without a rounding error" should "contain a total of all votes" in {
    assert(testVoteCounts1.total === VoteCount(46))
  }

  it can "be given a rounding error" in {
    val expectedVoteCountsWithRoundingError = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple      -> VoteCount(32),
        Banana     -> VoteCount(1),
        Pear       -> VoteCount(5),
        Strawberry -> VoteCount(6),
      ),
      exhausted = VoteCount(2),
      roundingError = VoteCount(5),
    )

    val actualVoteCountsWithRoundingError = testVoteCounts1.withRoundingError(VoteCount(5))

    assert(actualVoteCountsWithRoundingError === expectedVoteCountsWithRoundingError)
  }

  it can "be added to another instance" in {
    val actualVoteCounts = testVoteCounts1 + testVoteCounts2

    val expectedVoteCounts = CandidateVoteCountsSansRoundingError[Fruit](
      perCandidate = Map(
        Apple      -> VoteCount(30),
        Banana     -> VoteCount(6),
        Pear       -> VoteCount(8),
        Strawberry -> VoteCount(10),
      ),
      exhausted = VoteCount(4),
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

  it can "be built from some candidate vote counts with a rounding error" in {
    val withRoundingError = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple      -> VoteCount(32),
        Banana     -> VoteCount(1),
        Pear       -> VoteCount(5),
        Strawberry -> VoteCount(6),
      ),
      exhausted = VoteCount(2),
      roundingError = VoteCount(NumPapers(0), NumVotes(2)),
    )

    val actualWithoutRoundingError = CandidateVoteCountsSansRoundingError.from(withRoundingError)

    val expectedWithoutRoundingError = testVoteCounts1

    assert(actualWithoutRoundingError === expectedWithoutRoundingError)
  }

}
