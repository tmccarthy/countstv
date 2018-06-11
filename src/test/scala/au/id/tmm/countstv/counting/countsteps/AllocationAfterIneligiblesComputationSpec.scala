package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.fixtures.CountFixture
import au.id.tmm.countstv.counting.{AssignedPaperBundle, CountAction, PaperBundle}
import au.id.tmm.countstv.model.CandidateDistributionReason.Exclusion
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.AllocationAfterIneligibles
import au.id.tmm.countstv.model.values.{Count, Ordinal, TransferValue}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, VoteCount}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.parallel.immutable.ParSet

class AllocationAfterIneligiblesComputationSpec extends ImprovedFlatSpec {

  "an allocation after ineligibles when there are no ineligible candidates" should "have the right paper bundles" in {
    val fixture = CountFixture.withFinalElection

    val actualPaperBundles = fixture.contextAfterIneligibles.paperBundles

    val expectedPaperBundles = ParSet[PaperBundle[Fruit]](
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Banana).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Mango).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Pear).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Raspberry).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Strawberry).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Watermelon).get, PaperBundle.Origin.InitialAllocation),
    )

    assert(actualPaperBundles === expectedPaperBundles)
  }

  it should "have the right count step" in {
    val fixture = CountFixture.withFinalElection

    val actualCountStep = fixture.contextAfterIneligibles.mostRecentCountStep

    val expectedCountStep = AllocationAfterIneligibles(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Remaining,
        Watermelon -> Excluded(Ordinal.first, Count(1)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(10),
          Banana -> VoteCount(6),
          Mango -> VoteCount(9),
          Pear -> VoteCount(9),
          Raspberry -> VoteCount(7),
          Strawberry -> VoteCount(5),
          Watermelon -> VoteCount(4),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount.zero,
      ),
      transfersDueToIneligibles = Map.empty[Fruit, CandidateVoteCounts[Fruit]],
    )

    assert(actualCountStep == expectedCountStep)
  }

  it should "have the right next action" in {
    val fixture = CountFixture.withFinalElection

    val actualNextAction = fixture.contextAfterIneligibles.nextAction

    val expectedNextAction = CountAction.DistributeFromCandidate(Watermelon, Exclusion)

    assert(actualNextAction === expectedNextAction)
  }

  it should "have the right total vote count" in {
    val fixture = CountFixture.withFinalElection

    val actualTotalVoteCount = fixture.contextAfterIneligibles.mostRecentCountStep.candidateVoteCounts.total

    val expectedTotalVoteCount = VoteCount(fixture.numPapers.asLong)

    assert(actualTotalVoteCount === expectedTotalVoteCount)
  }

  "an allocation after ineligibles when there are two ineligible candidates" should "have the right paper bundles" in {
    val fixture = CountFixture.withTwoIneligibleCandidates

    val actualPaperBundles = fixture.contextAfterIneligibles.paperBundles

    val expectedPaperBundles = ParSet[PaperBundle[Fruit]](
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple, Banana).get, PaperBundle.Origin.IneligibleCandidate(Apple)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple, Pear).get, PaperBundle.Origin.IneligibleCandidate(Apple)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple, Mango).get, PaperBundle.Origin.IneligibleCandidate(Apple)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple, Raspberry).get, PaperBundle.Origin.IneligibleCandidate(Apple)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple, Strawberry, Pear).get, PaperBundle.Origin.IneligibleCandidate(Apple)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple, Strawberry, Watermelon).get, PaperBundle.Origin.IneligibleCandidate(Apple)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple, Strawberry, Mango).get, PaperBundle.Origin.IneligibleCandidate(Apple)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple, Watermelon).get, PaperBundle.Origin.IneligibleCandidate(Apple)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Banana).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Mango).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Pear).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Raspberry).get, PaperBundle.Origin.InitialAllocation),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Strawberry, Apple, Pear).get, PaperBundle.Origin.IneligibleCandidate(Strawberry)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Strawberry, Banana).get, PaperBundle.Origin.IneligibleCandidate(Strawberry)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Strawberry, Raspberry).get, PaperBundle.Origin.IneligibleCandidate(Strawberry)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Strawberry, Watermelon).get, PaperBundle.Origin.IneligibleCandidate(Strawberry)),
      AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Watermelon).get, PaperBundle.Origin.InitialAllocation),
    )

    assert(actualPaperBundles === expectedPaperBundles)
  }

  it should "have the right count step" in {
    val fixture = CountFixture.withTwoIneligibleCandidates

    val actualCountStep = fixture.contextAfterIneligibles.mostRecentCountStep

    val expectedCountStep = AllocationAfterIneligibles(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Ineligible,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Ineligible,
        Watermelon -> Excluded(Ordinal.first, Count(1)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(0),
          Banana -> VoteCount(9),
          Mango -> VoteCount(11),
          Pear -> VoteCount(13),
          Raspberry -> VoteCount(10),
          Strawberry -> VoteCount(0),
          Watermelon -> VoteCount(7),
        ),
        exhausted = VoteCount(0),
        roundingError = VoteCount(0),
      ),
      transfersDueToIneligibles = Map[Fruit, CandidateVoteCounts[Fruit]](
        Strawberry -> CandidateVoteCounts(
          perCandidate = Map(
            Apple -> VoteCount(0),
            Banana -> VoteCount(2),
            Mango -> VoteCount(0),
            Pear -> VoteCount(1),
            Raspberry -> VoteCount(1),
            Strawberry -> VoteCount(0),
            Watermelon -> VoteCount(1),
          ),
          exhausted = VoteCount(0),
          roundingError = VoteCount(0),
        ),
        Apple -> CandidateVoteCounts(
          perCandidate = Map(
            Apple -> VoteCount(0),
            Banana -> VoteCount(1),
            Mango -> VoteCount(2),
            Pear -> VoteCount(3),
            Raspberry -> VoteCount(2),
            Strawberry -> VoteCount(0),
            Watermelon -> VoteCount(2),
          ),
          exhausted = VoteCount(0),
          roundingError = VoteCount(0),
        ),
      ),
    )

    assert(actualCountStep == expectedCountStep)
  }

  it should "have the right next action" in {
    val fixture = CountFixture.withTwoIneligibleCandidates

    val actualNextAction = fixture.contextAfterIneligibles.nextAction

    val expectedNextAction = CountAction.DistributeFromCandidate(Watermelon, reason = Exclusion)

    assert(actualNextAction === expectedNextAction)
  }

  it should "have the right total vote count" in {
    val fixture = CountFixture.withTwoIneligibleCandidates

    val actualTotalVoteCount = fixture.contextAfterIneligibles.mostRecentCountStep.candidateVoteCounts.total

    val expectedTotalVoteCount = VoteCount(fixture.numPapers.asLong)

    assert(actualTotalVoteCount === expectedTotalVoteCount)
  }

}
