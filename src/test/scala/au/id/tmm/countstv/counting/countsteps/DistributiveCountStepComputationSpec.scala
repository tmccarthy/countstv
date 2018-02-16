package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountContext, DistributionCountStep}
import au.id.tmm.countstv.model.values._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.immutable.{Bag, HashedBagConfiguration, Queue}

class DistributiveCountStepComputationSpec extends ImprovedFlatSpec {

  private implicit val bagConfig: HashedBagConfiguration[PaperBundle[Fruit]] = PaperBundle.bagConfiguration[Fruit]

  private val testPreferenceTree = PreferenceTree.from[Fruit](
    Vector(Apple, Banana, Strawberry, Pear, Raspberry, Mango, Watermelon),
    Vector(Apple, Pear, Mango, Strawberry, Banana, Raspberry, Watermelon),
    Vector(Apple, Pear, Mango, Strawberry, Raspberry, Watermelon, Banana),
    Vector(Apple, Mango, Watermelon, Pear, Banana, Strawberry, Raspberry),
    Vector(Apple, Raspberry, Mango, Strawberry, Pear, Banana, Watermelon),
    Vector(Apple, Raspberry, Mango, Pear, Strawberry, Banana, Watermelon),
    Vector(Apple, Strawberry, Pear, Raspberry, Watermelon, Mango, Banana),
    Vector(Apple, Strawberry, Watermelon, Raspberry, Pear, Mango, Banana),
    Vector(Apple, Strawberry, Mango, Raspberry, Watermelon, Pear, Banana),
    Vector(Apple, Watermelon, Banana, Pear, Strawberry, Mango, Raspberry),
    Vector(Banana, Apple, Strawberry, Mango, Raspberry, Watermelon, Pear),
    Vector(Banana, Apple, Raspberry, Mango, Watermelon, Pear, Strawberry),
    Vector(Banana, Apple, Watermelon, Raspberry, Pear, Strawberry, Mango),
    Vector(Banana, Raspberry, Pear, Mango, Strawberry, Apple, Watermelon),
    Vector(Banana, Strawberry, Apple, Watermelon, Raspberry, Pear, Mango),
    Vector(Banana, Watermelon, Apple, Mango, Raspberry, Strawberry, Pear),
    Vector(Pear, Apple, Banana, Mango, Watermelon, Raspberry, Strawberry),
    Vector(Pear, Banana, Apple, Mango, Strawberry, Watermelon, Raspberry),
    Vector(Pear, Raspberry, Banana, Strawberry, Watermelon, Apple, Mango),
    Vector(Pear, Raspberry, Strawberry, Mango, Watermelon, Apple, Banana),
    Vector(Pear, Strawberry, Raspberry, Banana, Apple, Mango, Watermelon),
    Vector(Pear, Strawberry, Apple, Mango, Watermelon, Banana, Raspberry),
    Vector(Pear, Strawberry, Raspberry, Banana, Mango, Apple, Watermelon),
    Vector(Pear, Watermelon, Banana, Mango, Strawberry, Raspberry, Apple),
    Vector(Pear, Watermelon, Mango, Apple, Strawberry, Banana, Raspberry),
    Vector(Mango, Apple, Pear, Strawberry, Banana, Raspberry, Watermelon),
    Vector(Mango, Apple, Watermelon, Raspberry, Pear, Strawberry, Banana),
    Vector(Mango, Apple, Pear, Raspberry, Watermelon, Banana, Strawberry),
    Vector(Mango, Apple, Watermelon, Raspberry, Strawberry, Pear, Banana),
    Vector(Mango, Banana, Apple, Pear, Watermelon, Strawberry, Raspberry),
    Vector(Mango, Banana, Strawberry, Pear, Apple, Raspberry, Watermelon),
    Vector(Mango, Pear, Raspberry, Strawberry, Watermelon, Banana, Apple),
    Vector(Mango, Watermelon, Pear, Raspberry, Apple, Banana, Strawberry),
    Vector(Mango, Watermelon, Pear, Banana, Strawberry, Raspberry, Apple),
    Vector(Raspberry, Banana, Watermelon, Strawberry, Pear, Apple, Mango),
    Vector(Raspberry, Mango, Pear, Watermelon, Banana, Apple, Strawberry),
    Vector(Raspberry, Mango, Apple, Pear, Banana, Watermelon, Strawberry),
    Vector(Raspberry, Mango, Banana, Apple, Watermelon, Strawberry, Pear),
    Vector(Raspberry, Strawberry, Apple, Pear, Watermelon, Mango, Banana),
    Vector(Raspberry, Watermelon, Strawberry, Apple, Banana, Pear, Mango),
    Vector(Raspberry, Watermelon, Pear, Apple, Banana, Strawberry, Mango),
    Vector(Strawberry, Apple, Pear, Watermelon, Mango, Raspberry, Banana),
    Vector(Strawberry, Banana, Apple, Mango, Watermelon, Raspberry, Pear),
    Vector(Strawberry, Banana, Mango, Pear, Apple, Watermelon, Raspberry),
    Vector(Strawberry, Raspberry, Apple, Banana, Mango, Pear, Watermelon),
    Vector(Strawberry, Watermelon, Raspberry, Mango, Apple, Pear, Banana),
    Vector(Watermelon, Apple, Raspberry, Mango, Banana, Pear, Strawberry),
    Vector(Watermelon, Pear, Apple, Banana, Raspberry, Mango, Strawberry),
    Vector(Watermelon, Pear, Apple, Mango, Raspberry, Banana, Strawberry),
    Vector(Watermelon, Mango, Pear, Raspberry, Apple, Banana, Strawberry),
  )

  private val candidateStatuses = CandidateStatuses[Fruit](
    Apple -> Remaining,
    Banana -> Remaining,
    Pear -> Remaining,
    Strawberry -> Remaining,
    Mango -> Remaining,
    Raspberry -> Remaining,
    Watermelon -> Remaining,
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
  ).anyOutcome

  "count step 2, when Watermelon is excluded" should "have the correct number of formal papers" in {
    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)

    assert(actualContext.map(_.numFormalPapers) === ProbabilityMeasure.always(rootBundle.numPapers))
  }

  it should "have the correct number of vacancies" in {
    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)

    assert(actualContext.map(_.numVacancies) === ProbabilityMeasure.always(numVacancies))
  }

  it should "have the correct paper bundles" in {
    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)

    val expectedPaperBundles = Bag[PaperBundle[Fruit]](
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Apple).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Pear).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Pear).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon, Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Raspberry).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Mango).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Apple).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon, Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Banana).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Mango).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon, Count(2)),
      ),
    )

    assert(actualContext.map(_.paperBundles) === ProbabilityMeasure.always(expectedPaperBundles))
  }

  it should "have produced the correct distribution step" in {
    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(2),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Remaining,
        Watermelon -> Excluded(0, Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts(
        perCandidate = Map(
          Apple -> VoteCount(11),
          Banana -> VoteCount(6),
          Mango -> VoteCount(10),
          Pear -> VoteCount(11),
          Raspberry -> VoteCount(7),
          Strawberry -> VoteCount(5),
          Watermelon -> VoteCount(0),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount.zero,
      ),
      distributionSource = DistributionCountStep.Source(
        Watermelon,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(0)),
        transferValue = TransferValue(1),
      )
    )

    assert(actualContext.map(_.mostRecentCountStep) === ProbabilityMeasure.always(expectedCountStep))
  }

  it should "have no current distribution" in {
    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)

    assert(actualContext.map(_.currentDistribution) === ProbabilityMeasure.always(None))
  }

  "count step 3, when Strawberry is excluded" should "have the correct paper bundles" in {
    val contextAfterFirstDistribution = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)
      .asMap.keys.head

    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterFirstDistribution)

    val expectedPaperBundles = Bag[PaperBundle[Fruit]](
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Pear).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Apple).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry, Raspberry).get,
        PaperBundle.Origin.ExcludedCandidate(Strawberry,Count(3)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Raspberry).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Apple).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon,Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Banana).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Mango).get,
        PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry, Apple).get,
        PaperBundle.Origin.ExcludedCandidate(Strawberry,Count(3)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry, Watermelon, Raspberry).get,
        PaperBundle.Origin.ExcludedCandidate(Strawberry,Count(3)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Mango).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon,Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Watermelon, Pear).get,
        PaperBundle.Origin.ExcludedCandidate(Watermelon,Count(2)),
      ),
      AssignedPaperBundle[Fruit](
        TransferValue(1.0),
        testPreferenceTree.childFor(Strawberry, Banana).get,
        PaperBundle.Origin.ExcludedCandidate(Strawberry,Count(3)),
      ),
    )

    assert(actualContext.map(_.paperBundles) === ProbabilityMeasure.always(expectedPaperBundles))
  }

  it should "have produced the correct count step" in {
    val contextAfterFirstDistribution = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)
      .asMap.keys.head

    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterFirstDistribution)

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(3),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Excluded(1,Count(3)),
        Watermelon -> Excluded(0,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(12),
          Banana -> VoteCount(8),
          Mango -> VoteCount(10),
          Pear -> VoteCount(11),
          Raspberry -> VoteCount(9),
          Strawberry -> VoteCount(0),
          Watermelon -> VoteCount(0),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount.zero,
      ),
      distributionSource = DistributionCountStep.Source(
        Strawberry,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(0)),
        transferValue = TransferValue(1),
      )
    )

    assert(actualContext.map(_.mostRecentCountStep) === ProbabilityMeasure.always(expectedCountStep))
  }

  it should "have no current distribution" in {
    val contextAfterFirstDistribution = DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)
      .asMap.keys.head

    val actualContext = DistributiveCountStepComputation.computeNextContext(contextAfterFirstDistribution)

    assert(actualContext.map(_.currentDistribution) === ProbabilityMeasure.always(None))
  }

  "count 4, where Apple is elected" should "have produced the correct count step" in {
    val actualContext = (
      for {
        contextAfterCount2 <- DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)
        contextAfterCount3 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount2)
        contextAfterCount4 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount3)
      } yield contextAfterCount4
      ).anyOutcome

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(4),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(0,Count(4)),
        Banana -> Excluded(2,Count(4)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Excluded(1,Count(3)),
        Watermelon -> Excluded(0,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(18), NumVotes(18.0)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0.0)),
          Mango -> VoteCount(NumPapers(11), NumVotes(11.0)),
          Pear -> VoteCount(NumPapers(11), NumVotes(11.0)),
          Raspberry -> VoteCount(NumPapers(10), NumVotes(10.0)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0.0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0.0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(0.0))
      ),
      distributionSource = DistributionCountStep.Source(
        Banana,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(0), Count(3)),
        transferValue = TransferValue(1),
      )
    )

    assert(actualContext.mostRecentCountStep === expectedCountStep)
  }

  "count 5, where Apple is being distributed" should "have produced the correct count step" in {
    val actualContext = (
      for {
        contextAfterCount2 <- DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)
        contextAfterCount3 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount2)
        contextAfterCount4 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount3)
        contextAfterCount5 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount4)
      } yield contextAfterCount5
      ).anyOutcome

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(5),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(0,Count(4)),
        Banana -> Excluded(2,Count(4)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Remaining,
        Strawberry -> Excluded(1,Count(3)),
        Watermelon -> Excluded(0,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(0), NumVotes(17.0)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0.0)),
          Mango -> VoteCount(NumPapers(16), NumVotes(11.0)),
          Pear -> VoteCount(NumPapers(17), NumVotes(11.0)),
          Raspberry -> VoteCount(NumPapers(17), NumVotes(10.0)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0.0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0.0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(-1.0))
      ),
      distributionSource = DistributionCountStep.Source(
        Apple,
        CandidateDistributionReason.Election,
        sourceCounts = Set(Count(0), Count(2), Count(3), Count(4)),
        transferValue = TransferValue(1d / 18d),
      )
    )

    assert(actualContext.mostRecentCountStep === expectedCountStep)
  }

  "count 6, where Raspberry is excluded" should "have produced the correct count step" in {
    val actualContext = (
      for {
        contextAfterCount2 <- DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)
        contextAfterCount3 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount2)
        contextAfterCount4 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount3)
        contextAfterCount5 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount4)
        contextAfterCount6 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount5)
      } yield contextAfterCount6
      ).anyOutcome

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(6),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(0,Count(4)),
        Banana -> Excluded(2,Count(4)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Excluded(3,Count(6)),
        Strawberry -> Excluded(1,Count(3)),
        Watermelon -> Excluded(0,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(0), NumVotes(17.0)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0.0)),
          Mango -> VoteCount(NumPapers(21), NumVotes(16.0)),
          Pear -> VoteCount(NumPapers(22), NumVotes(16.0)),
          Raspberry -> VoteCount(NumPapers(7), NumVotes(0.0)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0.0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0.0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(-1.0))
      ),
      distributionSource = DistributionCountStep.Source(
        Raspberry,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(0), Count(3), Count(4)),
        transferValue = TransferValue(1),
      )
    )

    assert(actualContext.mostRecentCountStep === expectedCountStep)
  }

  it should "still have 1 more set of bundles to distribute from Raspberry" in {
    val actualContext = (
      for {
        contextAfterCount2 <- DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)
        contextAfterCount3 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount2)
        contextAfterCount4 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount3)
        contextAfterCount5 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount4)
        contextAfterCount6 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount5)
      } yield contextAfterCount6
      ).anyOutcome


    val expectedCurrentDistribution = CountContext.CurrentDistribution[Fruit](
      candidateBeingDistributed = Raspberry,
      distributionReason = CandidateDistributionReason.Exclusion,
      bundlesToDistribute = Queue(
        Bag(
          AssignedPaperBundle(
            TransferValue(1d / 18d),
            testPreferenceTree.childFor(Watermelon, Apple, Raspberry).get,
            PaperBundle.Origin.ElectedCandidate(Apple,TransferValueCoefficient(1d / 18d),Count(5))
          ),
          AssignedPaperBundle(
            TransferValue(1d / 18d),
            testPreferenceTree.childFor(Apple, Raspberry).get,
            PaperBundle.Origin.ElectedCandidate(Apple,TransferValueCoefficient(1d / 18d),Count(5))
          ),
          AssignedPaperBundle(
            TransferValue(1d / 18d),
            testPreferenceTree.childFor(Banana, Apple, Raspberry).get,
            PaperBundle.Origin.ElectedCandidate(Apple,TransferValueCoefficient(1d / 18d),Count(5))
          ),
          AssignedPaperBundle(
            TransferValue(1d / 18d),
            testPreferenceTree.childFor(Banana, Strawberry, Apple, Watermelon, Raspberry).get,
            PaperBundle.Origin.ElectedCandidate(Apple,TransferValueCoefficient(1d / 18d),Count(5))
          ),
          AssignedPaperBundle(
            TransferValue(1d / 18d),
            testPreferenceTree.childFor(Banana, Apple, Watermelon, Raspberry).get,
            PaperBundle.Origin.ElectedCandidate(Apple,TransferValueCoefficient(1d / 18d),Count(5))
          ),
          AssignedPaperBundle(
            TransferValue(1d / 18d),
            testPreferenceTree.childFor(Apple, Strawberry, Watermelon, Raspberry).get,
            PaperBundle.Origin.ElectedCandidate(Apple,TransferValueCoefficient(1d / 18d),Count(5)))
        )(PaperBundle.bagConfiguration[Fruit].asInstanceOf[HashedBagConfiguration[AssignedPaperBundle[Fruit]]]),
      ),
      transferValueCoefficient = TransferValueCoefficient(1),
    )

    assert(actualContext.currentDistribution === Some(expectedCurrentDistribution))
  }

  "count 7, where Raspberry's ballots are still being distributed" should "have produced the correct count step" in {
    val actualContext = (
      for {
        contextAfterCount2 <- DistributiveCountStepComputation.computeNextContext(contextAfterIneligibles)
        contextAfterCount3 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount2)
        contextAfterCount4 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount3)
        contextAfterCount5 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount4)
        contextAfterCount6 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount5)
        contextAfterCount7 <- DistributiveCountStepComputation.computeNextContext(contextAfterCount6)
      } yield contextAfterCount7
      ).anyOutcome

    val expectedCountStep = DistributionCountStep[Fruit](
      count = Count(7),
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Elected(0,Count(4)),
        Banana -> Excluded(2,Count(4)),
        Mango -> Remaining,
        Pear -> Remaining,
        Raspberry -> Excluded(3,Count(6)),
        Strawberry -> Excluded(1,Count(3)),
        Watermelon -> Excluded(0,Count(2)),
      ),
      candidateVoteCounts = CandidateVoteCounts[Fruit](
        perCandidate = Map(
          Apple -> VoteCount(NumPapers(0), NumVotes(17.0)),
          Banana -> VoteCount(NumPapers(0), NumVotes(0.0)),
          Mango -> VoteCount(NumPapers(25), NumVotes(16.0)),
          Pear -> VoteCount(NumPapers(25), NumVotes(16.0)),
          Raspberry -> VoteCount(NumPapers(0), NumVotes(0.0)),
          Strawberry -> VoteCount(NumPapers(0), NumVotes(0.0)),
          Watermelon -> VoteCount(NumPapers(0), NumVotes(0.0)),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount(NumPapers(0), NumVotes(-1.0))
      ),
      distributionSource = DistributionCountStep.Source(
        Raspberry,
        CandidateDistributionReason.Exclusion,
        sourceCounts = Set(Count(5)),
        transferValue = TransferValue(1d / 18d),
      )
    )

    assert(actualContext.mostRecentCountStep === expectedCountStep)
  }

  // TODO should indicate when counting is finished
  // TODO should reject a context that is marked as finished
  // TODO should check for terminal election conditions
}
