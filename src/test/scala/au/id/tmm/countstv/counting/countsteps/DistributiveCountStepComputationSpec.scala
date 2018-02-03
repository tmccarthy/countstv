package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountContext, DistributionCountStep}
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
    Vector(Strawberry, Apple, Pear),
    Vector(Strawberry, Apple, Pear, Banana),
    Vector(Strawberry, Banana, Apple, Pear),
    Vector(Strawberry, Pear, Apple, Banana),
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
          1,
          1d,
          PaperBundle.Origin.ExcludedCandidate(Strawberry, 2),
        ),
        AssignedPaperBundle(
          1d,
          testPreferenceTree.childFor(Apple).get,
          PaperBundle.Origin.InitialAllocation,
        ),
        AssignedPaperBundle(
          1d,
          testPreferenceTree.childFor(Banana).get,
          PaperBundle.Origin.InitialAllocation,
        ),
        AssignedPaperBundle(
          1d,
          testPreferenceTree.childFor(Pear).get,
          PaperBundle.Origin.InitialAllocation,
        ),
        AssignedPaperBundle(
          1d,
          testPreferenceTree.childFor(Strawberry, Apple).get,
          PaperBundle.Origin.ExcludedCandidate(Strawberry, 2),
        ),
        AssignedPaperBundle(
          1d,
          testPreferenceTree.childFor(Strawberry, Banana).get,
          PaperBundle.Origin.ExcludedCandidate(Strawberry, 2),
        ),
        AssignedPaperBundle(
          1d,
          testPreferenceTree.childFor(Strawberry, Pear).get,
          PaperBundle.Origin.ExcludedCandidate(Strawberry, 2),
        ),
      )(bagConfig),
      mostRecentCountStep = DistributionCountStep(
        count = 2,
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Remaining,
          Banana -> Remaining,
          Pear -> Elected(ordinalElected = 0, electedAtCount = 2),
          Strawberry -> Excluded(0, 2),
        ),
        candidateVoteCounts = CandidateVoteCounts[Fruit](
          perCandidate = Map(
            Apple -> VoteCount(8, 8.0),
            Banana -> VoteCount(7, 7.0),
            Pear -> VoteCount(9, 9.0),
            Strawberry -> VoteCount.zero,
          ),
          exhausted = VoteCount(1, 1d),
          roundingError = VoteCount.zero,
        ),
        distributionSource = DistributionCountStep.Source(
          Strawberry,
          CandidateDistributionReason.Exclusion,
          sourceCounts = Set(0),
          transferValue = 1d,
        )
      ),
      currentDistribution = None,
    )

    assert(actualContext === ProbabilityMeasure.Always(expectedContext))
  }

}
