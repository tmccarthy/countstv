package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.values.Count
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, ProbabilityMeasure, VoteCount}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class ExcludedCandidateComputationsSpec extends ImprovedFlatSpec {

  private def testTiedExclusionComputation(
                                            candidateVoteCounts: Map[Fruit, Int],
                                            candidateStatuses: CandidateStatuses[Fruit],
                                            expectedExcluded: ProbabilityMeasure[Fruit],
                                          ): Unit = {
    val counts = CandidateVoteCounts[Fruit](
      perCandidate = candidateVoteCounts.mapValues(votes => VoteCount(votes)),
      exhausted = VoteCount.zero,
      roundingError = VoteCount.zero,
    )

    val numFormalPapers = counts.perCandidate.valuesIterator.foldLeft(VoteCount.zero)(_ + _).numPapers

    val actualExcludedCandidate = ExcludedCandidateComputations.computeExcluded(
      counts,
      candidateStatuses,
    )

    assert(actualExcludedCandidate === expectedExcluded)
  }

  private def testExclusionComputation(
                                        candidateVoteCounts: Map[Fruit, Int],
                                        candidateStatuses: CandidateStatuses[Fruit],
                                        expectedExcluded: Fruit,
                                      ): Unit = {
    testTiedExclusionComputation(
      candidateVoteCounts,
      candidateStatuses,
      ProbabilityMeasure.always(expectedExcluded),
    )
  }

  "identifying excluded candidates" should "throw when there are no remaining candidates" in {
    intercept[IllegalArgumentException] {
      testTiedExclusionComputation(
        candidateVoteCounts = Map(
          Apple -> 40,
          Banana -> 30,
          Pear -> 20,
          Strawberry -> 10,
        ),
        candidateStatuses = CandidateStatuses(
          Apple -> Elected(0, Count(1)),
          Banana -> Elected(1, Count(4)),
          Pear -> Excluded(0, Count(3)),
          Strawberry -> Excluded(1, Count(2)),
        ),
        expectedExcluded = ProbabilityMeasure.always(Apple),
      )
    }
  }

  it should "pick the remaining candidate with the least votes" in {
    testTiedExclusionComputation(
      candidateVoteCounts = Map(
        Apple -> 20,
        Banana -> 20,
        Pear -> 20,
        Strawberry -> 19,
      ),
      candidateStatuses = CandidateStatuses(
        Apple -> Remaining,
        Banana -> Remaining,
        Pear -> Remaining,
        Strawberry -> Remaining,
      ),
      expectedExcluded = ProbabilityMeasure.always(Strawberry),
    )
  }

}
