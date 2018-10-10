package au.id.tmm.countstv.counting.render

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.FullCountComputation
import au.id.tmm.countstv.counting.fixtures.CountFixture
import au.id.tmm.countstv.counting.render.CountStepRenderer.StepCandidate._
import au.id.tmm.countstv.counting.render.CountStepRenderer.StepComment._
import au.id.tmm.countstv.counting.render.CountStepRenderer._
import au.id.tmm.countstv.model.CandidateDistributionReason._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.VoteCount
import au.id.tmm.countstv.model.countsteps.DistributionCountStep
import au.id.tmm.countstv.model.values._
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
      expectedStepComment = StepComment.FinalElection(DupelessSeq(Pear)),
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
    ).onlyOutcomeUnsafe.countSteps

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

  it can "render all rows for a completed count" in {
    val fixture = CountFixture.withFourCandidates

    val completedCount = FullCountComputation.runCount(
      fixture.candidates,
      Set.empty,
      fixture.numVacancies,
      fixture.preferenceTree,
    ).onlyOutcomeUnsafe

    val actualRenderedRows = CountStepRenderer.renderRowsFor(completedCount).toList

    val expectedRenderedRows = List(
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(0),Candidate(Apple),VoteCount(NumPapers(19),NumVotes(19)),VoteCount(NumPapers(19),NumVotes(19)),TransferValue(1.0),Remaining,changedThisStep = false,InitialAllocation),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(0),Candidate(Banana),VoteCount(NumPapers(1),NumVotes(1)),VoteCount(NumPapers(1),NumVotes(1)),TransferValue(1.0),Remaining,changedThisStep = false,InitialAllocation),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(0),Candidate(Pear),VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(1.0),Remaining,changedThisStep = false,InitialAllocation),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(0),Candidate(Strawberry),VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(1.0),Remaining,changedThisStep = false,InitialAllocation),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(0),Exhausted,VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(1.0),Remaining,changedThisStep = false,InitialAllocation),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(0),RoundingError,VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(1.0),Remaining,changedThisStep = false,InitialAllocation),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(1),Candidate(Apple),VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(19),NumVotes(19)),TransferValue(1.0),Elected(Ordinal(0),Count(1)),changedThisStep = true,NextStepDistributing(Apple,Election,Count(2),TransferValue(0.631578947368421),Set(Count(0)))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(1),Candidate(Banana),VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(1),NumVotes(1)),TransferValue(1.0),Remaining,changedThisStep = false,NextStepDistributing(Apple,Election,Count(2),TransferValue(0.631578947368421),Set(Count(0)))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(1),Candidate(Pear),VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(1.0),Remaining,changedThisStep = false,NextStepDistributing(Apple,Election,Count(2),TransferValue(0.631578947368421),Set(Count(0)))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(1),Candidate(Strawberry),VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(1.0),Remaining,changedThisStep = false,NextStepDistributing(Apple,Election,Count(2),TransferValue(0.631578947368421),Set(Count(0)))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(1),Exhausted,VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(1.0),Remaining,changedThisStep = false,NextStepDistributing(Apple,Election,Count(2),TransferValue(0.631578947368421),Set(Count(0)))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(1),RoundingError,VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(1.0),Remaining,changedThisStep = false,NextStepDistributing(Apple,Election,Count(2),TransferValue(0.631578947368421),Set(Count(0)))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(2),Candidate(Apple),VoteCount(NumPapers(-19),NumVotes(-12)),VoteCount(NumPapers(0),NumVotes(7)),TransferValue(0.631578947368421),Elected(Ordinal(0),Count(1)),changedThisStep = false,FinalElection(DupelessSeq(Pear))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(2),Candidate(Banana),VoteCount(NumPapers(1),NumVotes(0)),VoteCount(NumPapers(2),NumVotes(1)),TransferValue(0.631578947368421),Remaining,changedThisStep = false,FinalElection(DupelessSeq(Pear))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(2),Candidate(Pear),VoteCount(NumPapers(18),NumVotes(11)),VoteCount(NumPapers(18),NumVotes(11)),TransferValue(0.631578947368421),Elected(Ordinal(1),Count(2)),changedThisStep = true,FinalElection(DupelessSeq(Pear))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(2),Candidate(Strawberry),VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(0.631578947368421),Remaining,changedThisStep = false,FinalElection(DupelessSeq(Pear))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(2),Exhausted,VoteCount(NumPapers(0),NumVotes(0)),VoteCount(NumPapers(0),NumVotes(0)),TransferValue(0.631578947368421),Remaining,changedThisStep = false,FinalElection(DupelessSeq(Pear))),
      RenderedRow(2,NumPapers(20),NumVotes(7),Count(2),RoundingError,VoteCount(NumPapers(0),NumVotes(1)),VoteCount(NumPapers(0),NumVotes(1)),TransferValue(0.631578947368421),Remaining,changedThisStep = false,FinalElection(DupelessSeq(Pear))),
    )

    assert(actualRenderedRows === expectedRenderedRows)
  }
}
