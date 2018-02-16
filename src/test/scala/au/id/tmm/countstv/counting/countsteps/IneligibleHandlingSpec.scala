package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.model.CandidateStatus.{Remaining, _}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.AllocationAfterIneligibles
import au.id.tmm.countstv.model.values.{Count, NumPapers, TransferValue}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.immutable.Bag

class IneligibleHandlingSpec extends ImprovedFlatSpec {

  private val testPreferenceTree = PreferenceTree.from[Fruit](
    Vector(Apple),
    Vector(Apple, Banana),
    Vector(Apple, Banana, Pear, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Apple, Pear, Banana),
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Strawberry, Banana, Pear),
    Vector(Apple, Strawberry, Pear),
    Vector(Banana, Apple, Strawberry, Pear),
    Vector(Banana, Strawberry, Apple, Pear),
    Vector(Banana, Strawberry, Pear, Apple),
    Vector(Banana, Strawberry, Pear, Apple),
    Vector(Pear, Apple),
    Vector(Pear, Apple, Banana, Strawberry),
    Vector(Pear, Apple, Banana, Strawberry),
    Vector(Pear, Banana, Apple, Strawberry),
    Vector(Pear, Banana, Strawberry, Apple),
    Vector(Pear, Banana, Strawberry, Apple),
    Vector(Pear, Strawberry, Banana, Apple),
    Vector(Strawberry),
    Vector(Strawberry, Apple, Pear),
    Vector(Strawberry, Apple, Pear, Banana),
    Vector(Strawberry, Banana, Apple, Pear),
    Vector(Strawberry, Pear, Apple, Banana),
  )

  "the step for handling ineligible candidates" should "mark candidates elected when there are no ineligible candidates" in {
    val numFormalPapers = NumPapers(25)
    val numVacancies = 2
    val quota = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Remaining,
      Banana -> Remaining,
      Pear -> Remaining,
      Strawberry -> Remaining,
    )

    val paperBundles = PaperBundle.rootBundleFor[Fruit](testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, candidateStatuses)

    val initialContext = InitialAllocationComputation.computeInitialContext[Fruit](
      candidateStatuses,
      rootPaperBundle = PaperBundle.rootBundleFor[Fruit](testPreferenceTree),
      numVacancies = numVacancies,
    )

    val actualContextAfterIneligibles = IneligibleHandling.computeContextAfterIneligibles[Fruit](initialContext)

    val expectedContextAfterIneligibles = initialContext.copy(
      previousCountSteps = initialContext.previousCountSteps :+
        AllocationAfterIneligibles(
          candidateStatuses = CandidateStatuses[Fruit](
            Apple -> Elected(0, Count(1)),
            Banana -> Remaining,
            Pear -> Remaining,
            Strawberry -> Remaining,
          ),
          candidateVoteCounts = initialContext.mostRecentCountStep.candidateVoteCounts,
          transfersDueToIneligibles = Map.empty[Fruit, CandidateVoteCounts[Fruit]],
        )
    )

    assert(actualContextAfterIneligibles === ProbabilityMeasure.always(expectedContextAfterIneligibles))
  }

  it should "distribute away from ineligible candidates" in {
    val numFormalPapers = NumPapers(25)
    val numVacancies = 2
    val quota = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Remaining,
      Banana -> Remaining,
      Pear -> Remaining,
      Strawberry -> CandidateStatus.Ineligible,
    )

    val initialContext = InitialAllocationComputation.computeInitialContext[Fruit](
      candidateStatuses,
      rootPaperBundle = PaperBundle.rootBundleFor[Fruit](testPreferenceTree),
      numVacancies = numVacancies,
    )

    val actualContextAfterIneligibles = IneligibleHandling.computeContextAfterIneligibles[Fruit](initialContext)

    val expectedContextAfterIneligibles = initialContext.copy(
      previousCountSteps = initialContext.previousCountSteps :+
        AllocationAfterIneligibles(
          candidateStatuses = CandidateStatuses[Fruit](
            Apple -> Elected(0, Count(1)),
            Banana -> Remaining,
            Pear -> Remaining,
            Strawberry -> CandidateStatus.Ineligible,
          ),
          candidateVoteCounts = CandidateVoteCounts[Fruit](
            perCandidate = Map(
              Apple -> VoteCount(11),
              Banana -> VoteCount(5),
              Pear -> VoteCount(8),
              Strawberry -> VoteCount.zero,
            ),
            exhausted = VoteCount(1),
            roundingError = VoteCount.zero,
          ),
          transfersDueToIneligibles = Map[Fruit, CandidateVoteCounts[Fruit]](
            Strawberry -> CandidateVoteCounts(
              perCandidate = Map(
                Apple -> VoteCount(2),
                Banana -> VoteCount(1),
                Pear -> VoteCount(1),
                Strawberry -> VoteCount.zero,
              ),
              exhausted = VoteCount(1),
              roundingError = VoteCount.zero,
            )
          )
        ),
      paperBundles = Bag[PaperBundle[Fruit]](
        AssignedPaperBundle(TransferValue(1.0), testPreferenceTree.childFor(Apple).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1.0), testPreferenceTree.childFor(Banana).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1.0), testPreferenceTree.childFor(Pear).get, PaperBundle.Origin.InitialAllocation),
        AssignedPaperBundle(TransferValue(1.0), testPreferenceTree.childFor(Strawberry, Apple).get, PaperBundle.Origin.IneligibleCandidate(Strawberry)),
        AssignedPaperBundle(TransferValue(1.0), testPreferenceTree.childFor(Strawberry, Banana).get, PaperBundle.Origin.IneligibleCandidate(Strawberry)),
        AssignedPaperBundle(TransferValue(1.0), testPreferenceTree.childFor(Strawberry, Pear).get, PaperBundle.Origin.IneligibleCandidate(Strawberry)),
        ExhaustedPaperBundle[Fruit](NumPapers(1), TransferValue(1.0), PaperBundle.Origin.IneligibleCandidate(Strawberry)),
      )(PaperBundle.bagConfiguration),
    )

    assert(actualContextAfterIneligibles === ProbabilityMeasure.always(expectedContextAfterIneligibles))
  }

}
