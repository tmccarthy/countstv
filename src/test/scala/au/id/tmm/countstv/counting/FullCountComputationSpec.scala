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
      Banana -> Excluded(Ordinal.third,Count(4)),
      Mango -> Remaining,
      Pear -> Elected(Ordinal.second,Count(8)),
      Raspberry -> Excluded(Ordinal.fourth,Count(6)),
      Strawberry -> Excluded(Ordinal.second,Count(3)),
      Watermelon -> Excluded(Ordinal.first,Count(2)),
    )

    assert(countSteps.last.candidateStatuses === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when we have ineligible candidates" in {
    val countSteps = runFullCountFor(CountFixture.withOneIneligibleCandidate).onlyOutcome

    val expectedFinalOutcomes = CandidateStatuses[Fruit](
      Apple -> Ineligible,
      Banana -> Excluded(Ordinal.second, Count(3)),
      Mango -> Remaining,
      Pear -> Elected(Ordinal.first, Count(4)),
      Raspberry -> Elected(Ordinal.second, Count(4)),
      Strawberry -> Excluded(Ordinal.third, Count(4)),
      Watermelon -> Excluded(Ordinal.first, Count(2)),
    )

    assert(countSteps.last.candidateStatuses === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when there is a tie" in {
    val countSteps = runFullCountFor(CountFixture.withATie)

    val expectedFinalOutcomes = ProbabilityMeasure.evenly(
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(4)),
        Banana -> Excluded(Ordinal(2),Count(4)),
        Mango -> Remaining,
        Strawberry -> Excluded(Ordinal(0),Count(2)),
        Pear -> Elected(Ordinal(1),Count(8)),
        Raspberry -> Excluded(Ordinal(3),Count(6)),
        Watermelon -> Excluded(Ordinal(1),Count(3)),
      ),
      CandidateStatuses(
        Apple -> Elected(Ordinal(0),Count(4)),
        Banana -> Excluded(Ordinal(2),Count(4)),
        Mango -> Remaining,
        Strawberry -> Excluded(Ordinal(1),Count(3)),
        Pear -> Elected(Ordinal(1),Count(8)),
        Raspberry -> Excluded(Ordinal(3),Count(6)),
        Watermelon -> Excluded(Ordinal(0),Count(2)),
      ),
    )

    assert(countSteps.map(_.last.candidateStatuses) === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when there is a final election step" in {
    val actualOutcome = runFullCountFor(CountFixture.withFinalElection).map(_.last.candidateStatuses)

    val expectedFinalOutcome = ProbabilityMeasure.Always(
      CandidateStatuses[Fruit](
        Apple -> Elected(Ordinal.first,Count(4)),
        Banana -> Excluded(Ordinal.third,Count(4)),
        Mango -> Remaining,
        Pear -> Elected(Ordinal.second,Count(8)),
        Raspberry -> Excluded(Ordinal.fourth,Count(6)),
        Strawberry -> Excluded(Ordinal.second,Count(3)),
        Watermelon -> Excluded(Ordinal.first,Count(2)),
      )
    )

    assert(actualOutcome === expectedFinalOutcome)
  }

  it can "not occur if the set of ineligible candidates is not a subset of the set of all candidates" in {
    intercept[IllegalArgumentException] {
      runFullCountFor(CountFixture.withInvalidIneligibleCandidates)
    }
  }
}
