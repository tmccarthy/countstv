package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.fixtures.CountFixture
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.CandidateStatuses
import au.id.tmm.countstv.model.countsteps.CountSteps
import au.id.tmm.countstv.model.values.{Count, Ordinal}
import au.id.tmm.utilities.probabilities.ProbabilityMeasure
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class FullCountComputationSpec extends ImprovedFlatSpec {

  private def runFullCountFor(countFixture: CountFixture): ProbabilityMeasure[CountSteps[Fruit]] = {
    FullCountComputation.runCount(
      countFixture.candidates,
      countFixture.ineligibleCandidates,
      countFixture.numVacancies,
      countFixture.preferenceTree,
    )
  }

  "a full vote count" should "produce the correct outcome" in {
    val countSteps = runFullCountFor(CountFixture.withOneRemainingCandidate).onlyOutcome

    val expectedFinalOutcomes = CandidateStatuses[Fruit](
      Apple -> Elected(Ordinal.first,Count(4)),
      Banana -> Excluded(Ordinal.third,Count(3)),
      Mango -> Remaining,
      Pear -> Elected(Ordinal.second,Count(8)),
      Raspberry -> Excluded(Ordinal.fourth,Count(5)),
      Strawberry -> Excluded(Ordinal.second,Count(2)),
      Watermelon -> Excluded(Ordinal.first,Count(1)),
    )

    assert(countSteps.last.candidateStatuses === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when we have ineligible candidates" in {
    val countSteps = runFullCountFor(CountFixture.withOneIneligibleCandidate).onlyOutcome

    val expectedFinalOutcomes = CandidateStatuses[Fruit](
      Apple -> Ineligible,
      Banana -> Excluded(Ordinal.second, Count(2)),
      Mango -> Remaining,
      Pear -> Elected(Ordinal.first, Count(4)),
      Raspberry -> Elected(Ordinal.second, Count(4)),
      Strawberry -> Excluded(Ordinal.third, Count(3)),
      Watermelon -> Excluded(Ordinal.first, Count(1)),
    )

    assert(countSteps.last.candidateStatuses === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when there is a tie after the ineligible handling" in {
    val countSteps = runFullCountFor(CountFixture.withATieAtTheIneligibleHandling)

    val expectedFinalOutcomes = ProbabilityMeasure.evenly(
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(4)),
        Banana -> Excluded(Ordinal(2),Count(3)),
        Mango -> Remaining,
        Strawberry -> Excluded(Ordinal(0),Count(1)),
        Pear -> Elected(Ordinal(1),Count(8)),
        Raspberry -> Excluded(Ordinal(3),Count(5)),
        Watermelon -> Excluded(Ordinal(1),Count(2)),
      ),
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(4)),
        Banana -> Excluded(Ordinal(2),Count(3)),
        Mango -> Remaining,
        Strawberry -> Excluded(Ordinal(1),Count(2)),
        Pear -> Elected(Ordinal(1),Count(8)),
        Raspberry -> Excluded(Ordinal(3),Count(5)),
        Watermelon -> Excluded(Ordinal(0),Count(1)),
      ),
    )

    assert(countSteps.map(_.last.candidateStatuses) === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when there is a tie during distribution" in {
    val countSteps = runFullCountFor(CountFixture.withATieDuringTheDistributionPhase)

    val expectedFinalOutcomes = ProbabilityMeasure.evenly(
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(4)),
        Banana -> Excluded(Ordinal(2),Count(3)),
        Mango -> Elected(Ordinal(1),Count(8)),
        Pear -> Remaining,
        Raspberry -> Excluded(Ordinal(3),Count(5)),
        Strawberry -> Excluded(Ordinal(1),Count(2)),
        Watermelon -> Excluded(Ordinal(0),Count(1)),
      ),
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(5)),
        Banana -> Excluded(Ordinal(3),Count(4)),
        Mango -> Elected(Ordinal(1),Count(7)),
        Pear -> Remaining,
        Raspberry -> Excluded(Ordinal(2),Count(3)),
        Strawberry -> Excluded(Ordinal(1),Count(2)),
        Watermelon -> Excluded(Ordinal(0),Count(1)),
      ),
    )

    assert(countSteps.map(_.last.candidateStatuses) === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when there is a final election step" in {
    val actualOutcome = runFullCountFor(CountFixture.withFinalElection).map(_.last.candidateStatuses)

    val expectedFinalOutcome = ProbabilityMeasure.Always(
      CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(3)),
        Mango -> Remaining,
        Pear -> Elected(Ordinal.second,Count(8)),
        Raspberry -> Excluded(Ordinal.fourth,Count(5)),
        Strawberry -> Excluded(Ordinal.second,Count(2)),
        Watermelon -> Excluded(Ordinal.first,Count(1)),
      )
    )

    assert(actualOutcome === expectedFinalOutcome)
  }

  it should "produce the correct outcome when there is a final election step electing all remaining candidates" in {
    val actualOutcome = runFullCountFor(CountFixture.withAVacancyForEachCandidate).map(_.last.candidateStatuses)

    val expectedFinalOutcome = ProbabilityMeasure.Always(
      CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(2)),
        Banana -> Elected(Ordinal.fifth, Count(2)),
        Mango -> Elected(Ordinal.second, Count(2)),
        Pear -> Elected(Ordinal.third, Count(2)),
        Raspberry -> Elected(Ordinal.fourth, Count(2)),
        Strawberry -> Elected(Ordinal.sixth, Count(2)),
        Watermelon -> Elected(Ordinal.seventh, Count(2)),
      )
    )

    assert(actualOutcome === expectedFinalOutcome)
  }
}
