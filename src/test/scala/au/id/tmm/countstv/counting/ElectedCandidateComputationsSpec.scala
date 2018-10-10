package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.values.{Count, NumPapers, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, VoteCount}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.probabilities.ProbabilityMeasure
import au.id.tmm.utilities.testing.ImprovedFlatSpec
import org.scalatest.Assertion

class ElectedCandidateComputationsSpec extends ImprovedFlatSpec {

  private def testNewlyExceedingQuota(
                                       candidateStatuses: CandidateStatuses[Fruit],
                                       numVacancies: Int = 2,
                                       currentCandidateVoteCounts: Map[Fruit, Int],
                                       previousCandidateVoteCounts: List[Map[Fruit, Int]] = List.empty,
                                       expectedResult: ProbabilityMeasure[DupelessSeq[Fruit]],
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

    val numFormalPapers = NumPapers(currentCandidateVoteCounts.values.sum)

    val quota = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

    val actualResult = ElectedCandidateComputations.newlyExceedingQuota(
      parsedCurrentCandidateVoteCounts,
      parsedPreviousCandidateVoteCounts,
      candidateStatuses,
      numVacancies,
      quota,
    )

    assert(actualResult === expectedResult)
  }

  "identifying candidates newly exceeding quota" should "return nothing when no candidates exceed quota" in {
    testNewlyExceedingQuota(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 4,
        Banana -> 4,
        Mango -> 4,
        Pear -> 4,
        Raspberry -> 4,
      ),
      expectedResult = ProbabilityMeasure.Always(DupelessSeq.empty[Fruit]),
    )
  }

  it should "return nothing when some candidates exceed quota, but none that are not already elected" in {
    testNewlyExceedingQuota(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 100,
        Banana -> 4,
        Mango -> 4,
        Pear -> 4,
        Raspberry -> 4,
      ),
      expectedResult = ProbabilityMeasure.Always(DupelessSeq.empty[Fruit]),
    )
  }

  it should "return an unelected candidate that exceeds quota" in {
    testNewlyExceedingQuota(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 100,
        Banana -> 4,
        Mango -> 4,
        Pear -> 4,
        Raspberry -> 4,
      ),
      expectedResult = ProbabilityMeasure.Always(DupelessSeq(Apple)),
    )
  }

  it should "return an unelected candidate whose vote count equals the quota" in {
    testNewlyExceedingQuota(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 3,
        Banana -> 1,
        Mango -> 1,
        Pear -> 1,
        Raspberry -> 1,
      ),
      expectedResult = ProbabilityMeasure.Always(DupelessSeq(Apple)),
    )
  }

  it should "return unelected candidates that exceed quota in order of the number of votes" in {
    testNewlyExceedingQuota(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 102,
        Banana -> 101,
        Mango -> 100,
        Pear -> 0,
        Raspberry -> 0,
      ),
      numVacancies = 3,
      expectedResult = ProbabilityMeasure.Always(DupelessSeq(Banana, Mango)),
    )
  }

  it should "return all possibilities of candidate orders if there is a tie" in {
    testNewlyExceedingQuota(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 102,
        Banana -> 100,
        Mango -> 100,
        Pear -> 0,
        Raspberry -> 0,
      ),
      numVacancies = 3,
      expectedResult = ProbabilityMeasure.evenly(DupelessSeq(Banana, Mango), DupelessSeq(Mango, Banana)),
    )
  }

  it should "break ties with previous counts if there are any" in {
    testNewlyExceedingQuota(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 102,
        Banana -> 100,
        Mango -> 100,
        Pear -> 0,
        Raspberry -> 0,
      ),
      previousCandidateVoteCounts = List(
        Map(
          Apple -> 102,
          Banana -> 100,
          Mango -> 100,
          Pear -> 0,
          Raspberry -> 0,
        ),
        Map(
          Apple -> 102,
          Banana -> 101,
          Mango -> 100,
          Pear -> 0,
          Raspberry -> 0,
        ),
        Map(
          Apple -> 102,
          Banana -> 100,
          Mango -> 101,
          Pear -> 0,
          Raspberry -> 0,
        ),
      ),
      numVacancies = 3,
      expectedResult = ProbabilityMeasure.Always(DupelessSeq(Mango, Banana)),
    )
  }

}
