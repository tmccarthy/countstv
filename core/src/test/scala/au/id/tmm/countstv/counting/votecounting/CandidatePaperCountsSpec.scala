package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.VoteCount
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes, TransferValue}
import au.id.tmm.countstv.rules.RoundingRules
import org.scalatest.FlatSpec

class CandidatePaperCountsSpec extends FlatSpec {

  private implicit val roundingRules: RoundingRules = RoundingRules.AEC

  private val testPaperCounts1 = CandidatePaperCounts[Fruit](
    perCandidate = Map(
      Apple -> NumPapers(32),
      Banana -> NumPapers(1),
      Pear -> NumPapers(5),
      Strawberry -> NumPapers(6),
    ),
    exhausted = NumPapers(2),
  )

  private val testPaperCounts2 = CandidatePaperCounts[Fruit](
    perCandidate = Map(
      Apple -> NumPapers(4),
      Banana -> NumPapers(5),
      Pear -> NumPapers(5),
      Strawberry -> NumPapers(4),
    ),
    exhausted = NumPapers(6),
  )

  "a set of candidate paper counts" can "be initialised for a set of candidates" in {
    val candidates = Set(Apple, Banana, Pear, Strawberry)

    val actualPaperCounts = CandidatePaperCounts.zeroForEachOf(candidates)

    val expectedPaperCounts = CandidatePaperCounts(
      perCandidate = Map(
        Apple -> NumPapers(0),
        Banana -> NumPapers(0),
        Pear -> NumPapers(0),
        Strawberry -> NumPapers(0),
      ),
      exhausted = NumPapers(0),
    )

    assert(actualPaperCounts === expectedPaperCounts)
  }

  it can "be added to another instance" in {
    val actualPaperCounts = testPaperCounts1 + testPaperCounts2

    val expectedPaperCounts = CandidatePaperCounts[Fruit](
      perCandidate = Map(
        Apple -> NumPapers(36),
        Banana -> NumPapers(6),
        Pear -> NumPapers(10),
        Strawberry -> NumPapers(10),
      ),
      exhausted = NumPapers(8),
    )

    assert(actualPaperCounts === expectedPaperCounts)
  }

  it can "be subtracted from another instance" in {
    val actualPaperCounts = testPaperCounts1 - testPaperCounts2

    val expectedPaperCounts = CandidatePaperCounts[Fruit](
      perCandidate = Map(
        Apple -> NumPapers(28),
        Banana -> NumPapers(-4),
        Pear -> NumPapers(0),
        Strawberry -> NumPapers(2),
      ),
      exhausted = NumPapers(-4),
    )

    assert(actualPaperCounts === expectedPaperCounts)
  }

  it can "be multiplied by a transfer value to get a vote count" in {
    val base = CandidatePaperCounts[Fruit](
      perCandidate = Map(
        Apple -> NumPapers(28),
        Banana -> NumPapers(4),
        Pear -> NumPapers(0),
        Strawberry -> NumPapers(2),
      ),
      exhausted = NumPapers(4),
    )

    val actualVoteCounts = base * TransferValue(0.75d)

    val expectedVoteCounts = CandidateVoteCountsSansRoundingError[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(NumPapers(28), NumVotes(21)),
        Banana -> VoteCount(NumPapers(4), NumVotes(3)),
        Pear -> VoteCount(NumPapers(0), NumVotes(0)),
        Strawberry -> VoteCount(NumPapers(2), NumVotes(1)),
      ),
      exhausted = VoteCount(NumPapers(4), NumVotes(3)),
    )

    assert(actualVoteCounts === expectedVoteCounts)
  }

  it can "increment the count for a candidate" in {
    val actualPaperCounts = testPaperCounts1.increment(Banana, NumPapers(5))

    val expectedPaperCounts = CandidatePaperCounts[Fruit](
      perCandidate = Map(
        Apple -> NumPapers(32),
        Banana -> NumPapers(6),
        Pear -> NumPapers(5),
        Strawberry -> NumPapers(6),
      ),
      exhausted = NumPapers(2),
    )

    assert(actualPaperCounts === expectedPaperCounts)
  }

  it should "be left unchanged if asked to increment an uncounted candidate" in {
    assert(testPaperCounts1.increment(Watermelon, NumPapers(42)) === testPaperCounts1)
  }

}
