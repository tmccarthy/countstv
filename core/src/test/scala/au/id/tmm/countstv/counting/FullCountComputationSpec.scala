package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.fixtures.CountFixture
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.values.{Count, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatuses, CompletedCount}
import au.id.tmm.probabilitymeasure.ProbabilityMeasure
import org.scalatest.FlatSpec

class FullCountComputationSpec extends FlatSpec {

  private def runFullCountFor(countFixture: CountFixture): ProbabilityMeasure[CompletedCount[Fruit]] = {
    FullCountComputation.runCount(
      countFixture.countParams,
      countFixture.preferenceTree,
    )
  }

  "a full vote count" should "produce the correct outcome" in {
    val completedCount = runFullCountFor(CountFixture.withOneRemainingCandidate).onlyOutcomeUnsafe

    val expectedFinalOutcomes = CandidateStatuses[Fruit](
      Apple -> Elected(Ordinal.first,Count(4)),
      Banana -> Excluded(Ordinal.third,Count(3)),
      Mango -> Remaining,
      Pear -> Elected(Ordinal.second,Count(7)),
      Raspberry -> Excluded(Ordinal.fourth,Count(5)),
      Strawberry -> Excluded(Ordinal.second,Count(2)),
      Watermelon -> Excluded(Ordinal.first,Count(1)),
    )

    assert(completedCount.outcomes === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when we have ineligible candidates" in {
    val completedCount = runFullCountFor(CountFixture.withOneIneligibleCandidate).onlyOutcomeUnsafe

    val expectedFinalOutcomes = CandidateStatuses[Fruit](
      Apple -> Ineligible,
      Banana -> Excluded(Ordinal.second, Count(2)),
      Mango -> Remaining,
      Pear -> Elected(Ordinal.first, Count(4)),
      Raspberry -> Elected(Ordinal.second, Count(4)),
      Strawberry -> Excluded(Ordinal.third, Count(3)),
      Watermelon -> Excluded(Ordinal.first, Count(1)),
    )

    assert(completedCount.outcomes === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when there is a tie after the ineligible handling" in {
    val completedCount = runFullCountFor(CountFixture.withATieAtTheIneligibleHandling)

    val expectedFinalOutcomes = ProbabilityMeasure.evenly(
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(4)),
        Banana -> Excluded(Ordinal(2),Count(3)),
        Mango -> Remaining,
        Strawberry -> Excluded(Ordinal(0),Count(1)),
        Pear -> Elected(Ordinal(1),Count(7)),
        Raspberry -> Excluded(Ordinal(3),Count(5)),
        Watermelon -> Excluded(Ordinal(1),Count(2)),
      ),
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(4)),
        Banana -> Excluded(Ordinal(2),Count(3)),
        Mango -> Remaining,
        Strawberry -> Excluded(Ordinal(1),Count(2)),
        Pear -> Elected(Ordinal(1),Count(7)),
        Raspberry -> Excluded(Ordinal(3),Count(5)),
        Watermelon -> Excluded(Ordinal(0),Count(1)),
      ),
    )

    assert(completedCount.map(_.outcomes) === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when there is a tie during distribution" in {
    val completedCount = runFullCountFor(CountFixture.withATieDuringTheDistributionPhase)

    val expectedFinalOutcomes = ProbabilityMeasure.evenly(
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(4)),
        Banana -> Excluded(Ordinal(2),Count(3)),
        Mango -> Elected(Ordinal(1),Count(7)),
        Pear -> Remaining,
        Raspberry -> Excluded(Ordinal(3),Count(5)),
        Strawberry -> Excluded(Ordinal(1),Count(2)),
        Watermelon -> Excluded(Ordinal(0),Count(1)),
      ),
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(5)),
        Banana -> Excluded(Ordinal(3),Count(4)),
        Mango -> Elected(Ordinal(1),Count(6)),
        Pear -> Remaining,
        Raspberry -> Excluded(Ordinal(2),Count(3)),
        Strawberry -> Excluded(Ordinal(1),Count(2)),
        Watermelon -> Excluded(Ordinal(0),Count(1)),
      ),
    )

    assert(completedCount.map(_.outcomes) === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when there is a final election step" in {
    val actualOutcome = runFullCountFor(CountFixture.withFinalElection).map(_.outcomes)

    val expectedFinalOutcome = ProbabilityMeasure.Always(
      CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(3)),
        Mango -> Remaining,
        Pear -> Elected(Ordinal.second,Count(7)),
        Raspberry -> Excluded(Ordinal.fourth,Count(5)),
        Strawberry -> Excluded(Ordinal.second,Count(2)),
        Watermelon -> Excluded(Ordinal.first,Count(1)),
      )
    )

    assert(actualOutcome === expectedFinalOutcome)
  }

  it should "produce the correct outcome when there is a final election step electing all remaining candidates" in {
    val actualOutcome = runFullCountFor(CountFixture.withAVacancyForEachCandidate).map(_.outcomes)

    val expectedFinalOutcome = ProbabilityMeasure.Always(
      CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Elected(Ordinal.fifth, Count(1)),
        Mango -> Elected(Ordinal.second, Count(1)),
        Pear -> Elected(Ordinal.third, Count(1)),
        Raspberry -> Elected(Ordinal.fourth, Count(1)),
        Strawberry -> Elected(Ordinal.sixth, Count(1)),
        Watermelon -> Elected(Ordinal.seventh, Count(1)),
      )
    )

    assert(actualOutcome === expectedFinalOutcome)
  }

  it should "produce the correct outcome where all vacancies are filled by candidates exceeding quota after the initial allocation" in {
    val actualOutcome = runFullCountFor(CountFixture.whereEnoughCandidatesExceedQuotaWithoutDistribution).map(_.outcomes)

    val expectedFinalOutcome = ProbabilityMeasure.Always(
      CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first, Count(1)),
        Banana -> Elected(Ordinal.second, Count(1)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Remaining,
        Watermelon -> Remaining,
      )
    )

    assert(actualOutcome === expectedFinalOutcome)
  }
}
