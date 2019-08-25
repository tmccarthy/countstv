package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.fixtures.CountFixture
import au.id.tmm.countstv.counting.{AssignedPaperBundle, PaperBundle, QuotaComputation}
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountSteps, InitialAllocation}
import au.id.tmm.countstv.model.values.{NumPapers, TransferValue}
import au.id.tmm.countstv.rules.RoundingRules
import org.scalatest.FlatSpec

import scala.collection.parallel.immutable.ParSet

class InitialAllocationComputationSpec extends FlatSpec {

  private implicit val roundingRules: RoundingRules = RoundingRules.AEC

  "an initial allocation" can "be computed when there are no ineligible candidates" in {
    val fixture = CountFixture.withFinalElection

    val actualContext = fixture.initialContext

    val expectedContext = CountContext.Initial(
      NumPapers(50),
      numVacancies = 2,
      quota = QuotaComputation.computeQuota(numVacancies = 2, numFormalPapers = NumPapers(50)),
      paperBundles = ParSet[PaperBundle[Fruit]](
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Banana).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Mango).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Pear).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Raspberry).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Strawberry).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Watermelon).get, PaperBundle.Origin.InitialAllocation),
      ),
      previousCountSteps = CountSteps.Initial[Fruit](
        InitialAllocation(
          candidateStatuses = CandidateStatuses[Fruit](
            Apple -> Remaining,
            Banana -> Remaining,
            Mango -> Remaining,
            Pear -> Remaining,
            Raspberry -> Remaining,
            Strawberry -> Remaining,
            Watermelon -> Remaining,
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
          )
        )
      )
    )

    assert(actualContext === expectedContext)
  }

  it can "be computed when there are ineligible candidates" in {
    val fixture = CountFixture.withTwoIneligibleCandidates

    val actualContext = fixture.initialContext

    val expectedContext = CountContext.Initial(
      NumPapers(50),
      numVacancies = 2,
      quota = QuotaComputation.computeQuota(numVacancies = 2, numFormalPapers = NumPapers(50)),
      paperBundles = ParSet[PaperBundle[Fruit]](
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Apple).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Banana).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Mango).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Pear).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Raspberry).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Strawberry).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1), fixture.preferenceTree.childFor(Watermelon).get, PaperBundle.Origin.InitialAllocation),
      ),
      previousCountSteps = CountSteps.Initial[Fruit](
        InitialAllocation(
          candidateStatuses = CandidateStatuses[Fruit](
            Apple -> Ineligible,
            Banana -> Remaining,
            Mango -> Remaining,
            Pear -> Remaining,
            Raspberry -> Remaining,
            Strawberry -> Ineligible,
            Watermelon -> Remaining,
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
          )
        )
      )
    )

    assert(actualContext === expectedContext)
  }

}
