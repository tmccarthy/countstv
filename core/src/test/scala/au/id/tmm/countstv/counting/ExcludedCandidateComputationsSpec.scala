package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.values.{Count, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, VoteCount}
import au.id.tmm.probability.measure.ProbabilityMeasure
import org.scalatest.FlatSpec
import org.scalatest.Assertion

class ExcludedCandidateComputationsSpec extends FlatSpec {

  private def testComputeExcluded(
    candidateStatuses: CandidateStatuses[Fruit],
    currentCandidateVoteCounts: Map[Fruit, Int],
    previousCandidateVoteCounts: List[Map[Fruit, Int]] = List.empty,
    expectedResult: ProbabilityMeasure[Fruit],
  ): Assertion = {
    val parsedCurrentCandidateVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = currentCandidateVoteCounts.map { case (f, c) => f -> VoteCount(c) },
      exhausted = VoteCount.zero,
      roundingError = VoteCount.zero,
    )

    val parsedPreviousCandidateVoteCounts = previousCandidateVoteCounts.map { p =>
      CandidateVoteCounts[Fruit](
        perCandidate = p.map { case (f, c) => f -> VoteCount(c) },
        exhausted = VoteCount.zero,
        roundingError = VoteCount.zero,
      )
    }

    val actualResult = ExcludedCandidateComputations.computeExcluded(
      parsedCurrentCandidateVoteCounts,
      parsedPreviousCandidateVoteCounts,
      candidateStatuses,
    )

    assert(actualResult === expectedResult)
  }

  "identifying candidates to exclude" should "throw if there are no remaining candidates" in {
    intercept[IllegalArgumentException] {
      testComputeExcluded(
        candidateStatuses = CandidateStatuses[Fruit](
          Apple     -> Elected(Ordinal.first, Count(1)),
          Banana    -> Ineligible,
          Mango     -> Ineligible,
          Pear      -> Ineligible,
          Raspberry -> Excluded(Ordinal.first, Count(2)),
        ),
        currentCandidateVoteCounts = Map(
          Apple     -> 8,
          Banana    -> 7,
          Mango     -> 6,
          Pear      -> 5,
          Raspberry -> 4,
        ),
        expectedResult = ProbabilityMeasure.Always(Pear),
      )
    }
  }

  it should "return the remaining candidate with the least votes" in {
    testComputeExcluded(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple     -> Elected(Ordinal.first, Count(1)),
        Banana    -> Remaining,
        Mango     -> Remaining,
        Pear      -> Remaining,
        Raspberry -> Excluded(Ordinal.first, Count(2)),
      ),
      currentCandidateVoteCounts = Map(
        Apple     -> 8,
        Banana    -> 7,
        Mango     -> 6,
        Pear      -> 5,
        Raspberry -> 4,
      ),
      expectedResult = ProbabilityMeasure.Always(Pear),
    )
  }

  it should "return all remaining candidates if there is a tie" in {
    testComputeExcluded(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple     -> Elected(Ordinal.first, Count(1)),
        Banana    -> Remaining,
        Mango     -> Remaining,
        Pear      -> Remaining,
        Raspberry -> Excluded(Ordinal.first, Count(2)),
      ),
      currentCandidateVoteCounts = Map(
        Apple     -> 8,
        Banana    -> 6,
        Mango     -> 6,
        Pear      -> 6,
        Raspberry -> 4,
      ),
      expectedResult = ProbabilityMeasure.evenly(Banana, Mango, Pear),
    )
  }

  it should "break ties using previous counts" in {
    testComputeExcluded(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple     -> Elected(Ordinal.first, Count(1)),
        Banana    -> Remaining,
        Mango     -> Remaining,
        Pear      -> Remaining,
        Raspberry -> Excluded(Ordinal.first, Count(2)),
      ),
      currentCandidateVoteCounts = Map(
        Apple     -> 8,
        Banana    -> 6,
        Mango     -> 6,
        Pear      -> 6,
        Raspberry -> 4,
      ),
      previousCandidateVoteCounts = List(
        Map(
          Apple     -> 8,
          Banana    -> 6,
          Mango     -> 5,
          Pear      -> 6,
          Raspberry -> 4,
        ),
        Map(
          Apple     -> 8,
          Banana    -> 8,
          Mango     -> 6,
          Pear      -> 5,
          Raspberry -> 4,
        ),
        Map(
          Apple     -> 8,
          Banana    -> 6,
          Mango     -> 6,
          Pear      -> 6,
          Raspberry -> 4,
        ),
      ),
      expectedResult = ProbabilityMeasure.Always(Pear),
    )
  }
}
