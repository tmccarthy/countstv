package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountContext, DistributionCountStep}
import au.id.tmm.countstv.model.values._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.immutable.{Bag, HashedBagConfiguration}

class DistributiveCountStepComputationSpec extends ImprovedFlatSpec {

  private implicit val bagConfig: HashedBagConfiguration[PaperBundle[Fruit]] = PaperBundle.bagConfiguration[Fruit]

  private val testPreferenceTree = PreferenceTree.from[Fruit](
    Vector(Apple, Banana, Pear, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Apple, Pear, Banana),
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Strawberry, Pear),
    Vector(Banana),
    Vector(Banana, Apple),
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
    Vector(Pear, Strawberry, Banana, Apple),
    Vector(Strawberry),
    Vector(Strawberry, Banana, Apple, Pear),
    Vector(Strawberry, Pear, Apple, Banana),
    Vector(Strawberry, Pear),
    Vector(Strawberry, Pear),
  )

  private val candidateStatuses = CandidateStatuses[Fruit](
    Apple -> Remaining,
    Banana -> Remaining,
    Pear -> Remaining,
    Strawberry -> Remaining,
  )

  private val rootBundle = PaperBundle.rootBundleFor[Fruit](testPreferenceTree)

  private val numVacancies: Int = 2

  private val initialContext = InitialAllocationComputation.computeInitialContext(
    candidateStatuses,
    rootBundle,
    numVacancies,
  )

  private val contextAfterIneligibles = IneligibleHandling.computeContextAfterIneligibles(
    initialContext,
  ).asInstanceOf[ProbabilityMeasure.Always[CountContext[Fruit]]].outcome

  "a normal count step when no candidates are being elected or excluded, and no candidates have been elected" should
    "exclude the candidate with the fewest votes" in {

    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)

    val expectedContext = CountContext[Fruit](
      numFormalPapers = rootBundle.numPapers,
      numVacancies,
      paperBundles = Bag[PaperBundle[Fruit]](
        ExhaustedPaperBundle[Fruit](
          NumPapers(1),
          TransferValue(1d),
          PaperBundle.Origin.ExcludedCandidate(Strawberry, Count(2)),
        ),
        AssignedPaperBundle(
          TransferValue(1d),
          testPreferenceTree.childFor(Apple).get,
          PaperBundle.Origin.InitialAllocation,
        ),
        AssignedPaperBundle(
          TransferValue(1d),
          testPreferenceTree.childFor(Banana).get,
          PaperBundle.Origin.InitialAllocation,
        ),
        AssignedPaperBundle(
          TransferValue(1d),
          testPreferenceTree.childFor(Pear).get,
          PaperBundle.Origin.InitialAllocation,
        ),
        AssignedPaperBundle(
          TransferValue(1d),
          testPreferenceTree.childFor(Strawberry, Banana).get,
          PaperBundle.Origin.ExcludedCandidate(Strawberry, Count(2)),
        ),
        AssignedPaperBundle(
          TransferValue(1d),
          testPreferenceTree.childFor(Strawberry, Pear).get,
          PaperBundle.Origin.ExcludedCandidate(Strawberry, Count(2)),
        ),
      ),
      mostRecentCountStep = DistributionCountStep(
        count = Count(2),
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Remaining,
          Banana -> Remaining,
          Pear -> Elected(ordinalElected = 0, electedAtCount = Count(2)),
          Strawberry -> Excluded(0, Count(2)),
        ),
        candidateVoteCounts = CandidateVoteCounts[Fruit](
          perCandidate = Map(
            Apple -> VoteCount(6),
            Banana -> VoteCount(7),
            Pear -> VoteCount(11),
            Strawberry -> VoteCount.zero,
          ),
          exhausted = VoteCount(1),
          roundingError = VoteCount.zero,
        ),
        distributionSource = DistributionCountStep.Source(
          Strawberry,
          CandidateDistributionReason.Exclusion,
          sourceCounts = Set(Count(0)),
          transferValue = TransferValue(1),
        )
      ),
      currentDistribution = None,
    )

    assert(actualContext === ProbabilityMeasure.Always(expectedContext))
  }

  "a normal count step where no candidates are being elected or excluded, and a candidate has been elected" should
    "distribute from the elected candidate" in {
    val contextAfterFirstDistribution = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)
      .asMap.keys.head

    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterFirstDistribution)

    val expectedContext = CountContext[Fruit](
      numFormalPapers = rootBundle.numPapers,
      numVacancies,
      paperBundles = Bag[PaperBundle[Fruit]](
        ExhaustedPaperBundle[Fruit](
          NumPapers(1),
          TransferValue(1d),
          PaperBundle.Origin.ExcludedCandidate(Strawberry, Count(2)),
        ),
        ExhaustedPaperBundle[Fruit](
          NumPapers(2),
          TransferValue(2d / 11d),
          PaperBundle.Origin.ElectedCandidate(Pear, TransferValueCoefficient(2d / 11d), Count(3))
        ),
        AssignedPaperBundle(
          TransferValue(1d),
          testPreferenceTree.childFor(Apple).get,
          PaperBundle.Origin.InitialAllocation,
        ),
        AssignedPaperBundle(
          TransferValue(1d),
          testPreferenceTree.childFor(Banana).get,
          PaperBundle.Origin.InitialAllocation,
        ),
        AssignedPaperBundle(
          TransferValue(2d / 11d),
          testPreferenceTree.childFor(Pear, Apple).get,
          PaperBundle.Origin.ElectedCandidate(Pear, TransferValueCoefficient(2d / 11d), Count(3)),
        ),
        AssignedPaperBundle(
          TransferValue(2d / 11d),
          testPreferenceTree.childFor(Pear, Banana).get,
          PaperBundle.Origin.ElectedCandidate(Pear, TransferValueCoefficient(2d / 11d), Count(3)),
        ),
        AssignedPaperBundle(
          TransferValue(2d / 11d),
          testPreferenceTree.childFor(Pear, Strawberry, Banana).get,
          PaperBundle.Origin.ElectedCandidate(Pear, TransferValueCoefficient(2d / 11d), Count(3)),
        ),
        AssignedPaperBundle(
          TransferValue(1d),
          testPreferenceTree.childFor(Strawberry, Banana).get,
          PaperBundle.Origin.ExcludedCandidate(Strawberry, Count(2)),
        ),
        AssignedPaperBundle(
          TransferValue(2d / 11d),
          testPreferenceTree.childFor(Strawberry, Pear, Apple).get,
          PaperBundle.Origin.ElectedCandidate(Pear, TransferValueCoefficient(2d / 11d), Count(3)),
        ),
      ),
      mostRecentCountStep = DistributionCountStep(
        count = Count(3),
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Remaining,
          Banana -> Remaining,
          Pear -> Elected(ordinalElected = 0, electedAtCount = Count(2)),
          Strawberry -> Excluded(ordinalExcluded = 0, excludedAtCount = Count(2)),
        ),
        candidateVoteCounts = CandidateVoteCounts[Fruit](
          perCandidate = Map(
            Apple -> VoteCount(NumPapers(10), NumVotes(6.0)),
            Banana -> VoteCount(NumPapers(12), NumVotes(7.0)),
            Pear -> VoteCount(NumPapers(0), NumVotes(9.0)),
            Strawberry -> VoteCount.zero,
          ),
          exhausted = VoteCount(NumPapers(3), NumVotes(1d)),
          roundingError = VoteCount(NumPapers(0), NumVotes(-2.0)),
        ),
        distributionSource = DistributionCountStep.Source(
          Pear,
          CandidateDistributionReason.Election,
          sourceCounts = Set(Count(0), Count(2)),
          transferValue = TransferValue(2d / 11d),
        )
      ),
      currentDistribution = None,
    )

    assert(actualContext === ProbabilityMeasure.Always(expectedContext))
  }
}
