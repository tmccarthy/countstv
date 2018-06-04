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

  private def testFinallyElected(
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

    val actualResult = ElectedCandidateComputations.finallyElected(
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
      expectedResult = ProbabilityMeasure.always(DupelessSeq.empty[Fruit]),
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
      expectedResult = ProbabilityMeasure.always(DupelessSeq.empty[Fruit]),
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
      expectedResult = ProbabilityMeasure.always(DupelessSeq(Apple)),
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
      expectedResult = ProbabilityMeasure.always(DupelessSeq(Apple)),
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
      expectedResult = ProbabilityMeasure.always(DupelessSeq(Banana, Mango)),
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
      expectedResult = ProbabilityMeasure.always(DupelessSeq(Mango, Banana)),
    )
  }

  "identifying finally elected candidates" should "return nothing when the number of remaining candidates exceeds the " +
    "number of vacancies and we're not down to the last 2 candidates" in {
    testFinallyElected(
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
      expectedResult = ProbabilityMeasure.always(DupelessSeq.empty[Fruit]),
    )
  }

  it should "elect the higher candidate when 2 remain and there is 1 remaining vacancy" in {
    testFinallyElected(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Excluded(Ordinal.first, Count(2)),
        Raspberry -> Excluded(Ordinal.second, Count(3)),
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 4,
        Banana -> 3,
        Mango -> 2,
        Pear -> 0,
        Raspberry -> 0,
      ),
      expectedResult = ProbabilityMeasure.always(DupelessSeq(Banana)),
    )
  }

  it should "elect one of the two remaining candidates if they have the same number of votes and there is " +
    "1 vacancy remaining" in {
    testFinallyElected(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Excluded(Ordinal.first, Count(2)),
        Raspberry -> Excluded(Ordinal.second, Count(3)),
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 4,
        Banana -> 3,
        Mango -> 3,
        Pear -> 0,
        Raspberry -> 0,
      ),
      expectedResult = ProbabilityMeasure.evenly(DupelessSeq(Banana), DupelessSeq(Mango)),
    )
  }

  it should "break ties between 2 remaining candidates using previous counts" in {
    testFinallyElected(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Excluded(Ordinal.first, Count(2)),
        Raspberry -> Excluded(Ordinal.second, Count(3)),
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 4,
        Banana -> 3,
        Mango -> 3,
        Pear -> 0,
        Raspberry -> 0,
      ),
      previousCandidateVoteCounts = List(
        Map(
          Apple -> 4,
          Banana -> 3,
          Mango -> 3,
          Pear -> 0,
          Raspberry -> 0,
        ),
        Map(
          Apple -> 4,
          Banana -> 3,
          Mango -> 2,
          Pear -> 0,
          Raspberry -> 0,
        ),
        Map(
          Apple -> 4,
          Banana -> 2,
          Mango -> 3,
          Pear -> 0,
          Raspberry -> 0,
        ),
      ),
      expectedResult = ProbabilityMeasure.always(DupelessSeq(Mango)),
    )
  }

  // Special case elections

  it should "elect all candidates in ballot order when the number of remaining candidates is equal to the number " +
    "of unfilled vacancies" in {
    testFinallyElected(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Excluded(Ordinal.second, Count(3)),
        Raspberry -> Excluded(Ordinal.first, Count(2)),
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 10,
        Banana -> 9,
        Mango -> 8,
        Pear -> 7,
        Raspberry -> 6,
      ),
      numVacancies = 3,
      expectedResult = ProbabilityMeasure.always(DupelessSeq(Banana, Mango)),
    )
  }

  it should "return all possibilities if there is a tie between candidates when the number of remaining candidates " +
    "equals the number of unfilled vacancies" in {
    testFinallyElected(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Excluded(Ordinal.second, Count(3)),
        Raspberry -> Excluded(Ordinal.first, Count(2)),
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 10,
        Banana -> 8,
        Mango -> 8,
        Pear -> 7,
        Raspberry -> 6,
      ),
      numVacancies = 3,
      expectedResult = ProbabilityMeasure.evenly(DupelessSeq(Banana, Mango), DupelessSeq(Mango, Banana)),
    )
  }

  it should "break ties using previous counts when the number of remaining candidates equals the " +
    "number of unfilled vacancies" in {
    testFinallyElected(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Excluded(Ordinal.second, Count(3)),
        Raspberry -> Excluded(Ordinal.first, Count(2)),
      ),
      currentCandidateVoteCounts = Map(
        Apple -> 10,
        Banana -> 8,
        Mango -> 8,
        Pear -> 7,
        Raspberry -> 6,
      ),
      previousCandidateVoteCounts = List(
        Map(
          Apple -> 10,
          Banana -> 8,
          Mango -> 9,
          Pear -> 7,
          Raspberry -> 6,
        ),
        Map(
          Apple -> 10,
          Banana -> 9,
          Mango -> 8,
          Pear -> 7,
          Raspberry -> 6,
        ),
        Map(
          Apple -> 10,
          Banana -> 8,
          Mango -> 8,
          Pear -> 7,
          Raspberry -> 6,
        ),
      ),
      numVacancies = 3,
      expectedResult = ProbabilityMeasure.always(DupelessSeq(Banana, Mango)),
    )
  }

}
