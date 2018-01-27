package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, ProbabilityMeasure, VoteCount}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.testing.ImprovedFlatSpec
import spire.math.Rational

class ElectedCandidateComputationsSpec extends ImprovedFlatSpec {

  private def testElectionComputation(
                                       candidateVoteCounts: Map[Fruit, Int],
                                       candidateStatuses: CandidateStatuses[Fruit],
                                       numVacancies: Int,
                                       expectedElected: DupelessSeq[Fruit],
                                     ): Unit = {
    testTiedElectionComputation(
      candidateVoteCounts,
      candidateStatuses,
      numVacancies,
      ProbabilityMeasure.always(expectedElected),
    )
  }

  private def testTiedElectionComputation(
                                       candidateVoteCounts: Map[Fruit, Int],
                                       candidateStatuses: CandidateStatuses[Fruit],
                                       numVacancies: Int,
                                       expectedElected: ProbabilityMeasure[DupelessSeq[Fruit]],
                                     ): Unit = {
    val counts = CandidateVoteCounts[Fruit](
      perCandidate = candidateVoteCounts.mapValues(votes => VoteCount(votes, votes)),
      exhausted = VoteCount.zero,
      roundingError = VoteCount.zero,
    )

    val numFormalPapers = counts.perCandidate.valuesIterator.foldLeft(VoteCount.zero)(_ + _).numPapers

    val quota = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

    val actualElectedCandidates = ElectedCandidateComputations.computeElected(
      counts,
      candidateStatuses,
      numVacancies,
      quota,
    )

    assert(actualElectedCandidates === expectedElected)
  }

  "identifying elected candidates" should "be correct when there are no elected candidates" in {
    testElectionComputation(
      candidateVoteCounts = Map(
        Apple -> 42,
        Banana -> 32,
        Pear -> 10,
        Strawberry -> 2,
      ),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Pear -> Remaining,
        Strawberry -> Remaining,
      ),
      numVacancies = 2,
      expectedElected = DupelessSeq[Fruit](Apple, Banana),
    )
  }

  it should "be correct when there there is a tie for an elected candidate" in {
    testTiedElectionComputation(
      candidateVoteCounts = Map(
        Apple -> 42,
        Banana -> 42,
        Pear -> 10,
        Strawberry -> 2,
      ),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Pear -> Remaining,
        Strawberry -> Remaining,
      ),
      numVacancies = 3,
      expectedElected =
        ProbabilityMeasure(
          DupelessSeq[Fruit](Apple, Banana) -> Rational(1, 2),
          DupelessSeq[Fruit](Banana, Apple) -> Rational(1, 2),
        ),
    )
  }

  it should "be correct when there is an already elected candidate" in {
    testElectionComputation(
      candidateVoteCounts = Map(
        Apple -> 0,
        Banana -> 32,
        Pear -> 10,
        Strawberry -> 2,
      ),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(0, electedAtCount = 1),
        Banana -> Remaining,
        Pear -> Remaining,
        Strawberry -> Remaining,
      ),
      numVacancies = 2,
      expectedElected = DupelessSeq[Fruit](Apple, Banana),
    )
  }

  it should "be correct when an excluded candidate has more votes than any remaining candidate" in {
    // This case should never happen in a real count

    testElectionComputation(
      candidateVoteCounts = Map(
        Apple -> 0,
        Banana -> 52,
        Pear -> 10,
        Strawberry -> 2,
      ),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(0, electedAtCount = 1),
        Banana -> Excluded(2, excludedAtCount = 4),
        Pear -> Remaining,
        Strawberry -> Remaining,
      ),
      numVacancies = 2,
      expectedElected = DupelessSeq[Fruit](Apple),
    )
  }

  it should "be correct when there is only 1 remaining candidate and 1 remaining vacancy" in {
    testElectionComputation(
      candidateVoteCounts = Map(
        Apple -> 42,
        Banana -> 2,
        Pear -> 3,
        Strawberry -> 1,
      ),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(ordinalElected = 0, electedAtCount = 1),
        Banana -> Remaining,
        Pear -> Remaining,
        Strawberry -> Excluded(ordinalExcluded = 1, excludedAtCount = 3),
      ),
      numVacancies = 3,
      expectedElected = DupelessSeq[Fruit](Apple, Pear, Banana),
    )
  }

  it should "be correct when there are 2 tied candidates remaining and only 1 vacancy" in {
    testTiedElectionComputation(
      candidateVoteCounts = Map(
        Apple -> 0,
        Banana -> 40,
        Pear -> 40,
        Strawberry -> 0,
      ),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(ordinalElected = 0, electedAtCount = 1),
        Banana -> Remaining,
        Pear -> Remaining,
        Strawberry -> Excluded(ordinalExcluded = 1, excludedAtCount = 3),
      ),
      numVacancies = 2,
      expectedElected = ProbabilityMeasure(
        DupelessSeq[Fruit](Apple, Pear) -> Rational(1, 2),
        DupelessSeq[Fruit](Apple, Banana) -> Rational(1, 2),
      )
    )
  }

  it should "be correct when there are 2 remaining candidates and 2 of 3 vacancies have been filled" in {
    testElectionComputation(
      candidateVoteCounts = Map(
        Apple -> 42,
        Banana -> 2,
        Pear -> 3,
        Strawberry -> 1,
      ),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(ordinalElected = 0, electedAtCount = 1),
        Banana -> Elected(ordinalElected = 1, electedAtCount = 2),
        Pear -> Remaining,
        Strawberry -> Remaining,
      ),
      numVacancies = 3,
      expectedElected = DupelessSeq[Fruit](Apple, Banana),
    )
  }

  it should "be correct when all vacancies have been filled" in {
    testElectionComputation(
      candidateVoteCounts = Map(
        Apple -> 0,
        Banana -> 0,
        Pear -> 40,
        Strawberry -> 30,
      ),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(ordinalElected = 0, electedAtCount = 1),
        Banana -> Elected(ordinalElected = 1, electedAtCount = 2),
        Pear -> Remaining,
        Strawberry -> Remaining,
      ),
      numVacancies = 2,
      expectedElected = DupelessSeq[Fruit](Apple, Banana),
    )
  }
}
