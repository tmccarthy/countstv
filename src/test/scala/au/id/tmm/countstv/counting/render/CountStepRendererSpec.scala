package au.id.tmm.countstv.counting.render

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.FullCountComputation
import au.id.tmm.countstv.counting.fixtures.CountFixture
import au.id.tmm.countstv.counting.render.CountStepRenderer.StepComment
import au.id.tmm.countstv.model.CandidateDistributionReason.{Election, Exclusion}
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.VoteCount
import au.id.tmm.countstv.model.countsteps.DistributionCountStep
import au.id.tmm.countstv.model.values.{Count, TransferValue}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.testing.ImprovedFlatSpec
import org.scalatest.Assertion

class CountStepRendererSpec extends ImprovedFlatSpec {

  "the count step renderer" should "render an initial count step" in {
    testRenderedStep(
      fixture = CountFixture.withFinalElection,
      count = Count(0),
      expectedStepComment = StepComment.InitialAllocation,
    )
  }

  it should "render an allocation after ineligibles" in {
    testRenderedStep(
      fixture = CountFixture.withFinalElection,
      count = Count(1),
      expectedStepComment = StepComment.NextStepDistributing(Watermelon, Exclusion, Count(2), TransferValue(1), Set(Count(0))),
    )
  }

  it should "render a step before a distribution step" in {
    testRenderedStep(
      fixture = CountFixture.withFinalElection,
      count = Count(4),
      expectedStepComment = StepComment.NextStepDistributing(Apple, Election, Count(5), TransferValue(1d / 18d), Set(Count(0), Count(2), Count(3), Count(4)))
    )
  }

  it should "render a step before a step where a candidate is excluded with no votes" in {
    testRenderedStep(
      fixture = CountFixture.withVotelessCandidate,
      count = Count(1),
      expectedStepComment = StepComment.ExcludedNoVotes(Watermelon, Count(2)),
    )
  }

  it should "render a step before a step where a candidate is elected without a surplus" in {
    testRenderedStep(
      fixture = CountFixture.withElectionSansSurplus,
      count = Count(4),
      expectedStepComment = StepComment.ElectedNoSurplus(Apple, Count(5), Set(Count(0), Count(3), Count(4))),
    )
  }

  it should "render a step before a final election step" in {
    testRenderedStep(
      fixture = CountFixture.withFinalElection,
      count = Count(7),
      expectedStepComment = StepComment.FinalElection(DupelessSeq(Pear), Count(8)),
    )
  }

  it should "render a final election step" in {
    testRenderedStep(
      fixture = CountFixture.withFinalElection,
      count = Count(8),
      expectedStepComment = StepComment.AllVacanciesFilled,
    )
  }

  private def testRenderedStep(
                                fixture: CountFixture,
                                count: Count,
                                expectedStepComment: StepComment[Fruit],
                              ): Assertion = {
    val countSteps = FullCountComputation.runCount(
      fixture.candidates,
      Set.empty,
      fixture.numVacancies,
      fixture.preferenceTree,
    ).onlyOutcome

    val stepPrior = countSteps.lift(count.decrement)
    val step = countSteps(count)
    val stepAfter = countSteps.lift(count.increment)

    val actualRenderedStep = CountStepRenderer.renderRowsFor(fixture.numVacancies, fixture.numPapers, fixture.quota)(stepPrior, step, stepAfter)

    val expectedRenderedStep = fixture.candidates.toList.sorted.map { candidate =>
      CountStepRenderer.RenderedRow(
        fixture.numVacancies,
        fixture.numPapers,
        fixture.quota,
        count,
        candidate = CountStepRenderer.StepCandidate.Candidate(candidate),
        votesTransferred = step.candidateVoteCounts.perCandidate(candidate) - stepPrior.map(_.candidateVoteCounts.perCandidate(candidate)).getOrElse(VoteCount.zero),
        progressiveVoteTotal = step.candidateVoteCounts.perCandidate(candidate),
        transferValue = step match {
          case c: DistributionCountStep[Fruit] => c.distributionSource.transferValue
          case _ => TransferValue(1)
        },
        status = step.candidateStatuses.asMap(candidate),
        changedThisStep = stepPrior.exists(_.candidateStatuses.asMap(candidate) != step.candidateStatuses.asMap(candidate)),
        stepComment = expectedStepComment,
      )
    } :+
      CountStepRenderer.RenderedRow(
        fixture.numVacancies,
        fixture.numPapers,
        fixture.quota,
        count,
        CountStepRenderer.StepCandidate.Exhausted,
        step.candidateVoteCounts.exhausted - stepPrior.map(_.candidateVoteCounts.exhausted).getOrElse(VoteCount.zero),
        step.candidateVoteCounts.exhausted,
        step match {
          case c: DistributionCountStep[Fruit] => c.distributionSource.transferValue
          case _ => TransferValue(1)
        },
        status = Remaining,
        changedThisStep = false,
        stepComment = expectedStepComment,
      ) :+
      CountStepRenderer.RenderedRow(
        fixture.numVacancies,
        fixture.numPapers,
        fixture.quota,
        count,
        CountStepRenderer.StepCandidate.RoundingError,
        step.candidateVoteCounts.roundingError - stepPrior.map(_.candidateVoteCounts.roundingError).getOrElse(VoteCount.zero),
        step.candidateVoteCounts.roundingError,
        step match {
          case c: DistributionCountStep[Fruit] => c.distributionSource.transferValue
          case _ => TransferValue(1)
        },
        status = Remaining,
        changedThisStep = false,
        stepComment = expectedStepComment,
      )

    assert(actualRenderedStep === expectedRenderedStep)
  }

}
