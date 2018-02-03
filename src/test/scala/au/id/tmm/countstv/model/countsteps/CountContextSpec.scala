package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.immutable.{Bag, HashedBagConfiguration, Queue}

class CountContextSpec extends ImprovedFlatSpec {

  private implicit def bagConfiguration[A]: HashedBagConfiguration[A] =
    PaperBundle.bagConfiguration.asInstanceOf[HashedBagConfiguration[A]]

  private val testContext = CountContext(
    numFormalPapers = 40,
    numVacancies = 2,
    paperBundles = Bag.empty[PaperBundle[Fruit]],
    mostRecentCountStep = AllocationAfterIneligibles(
      candidateStatuses = CandidateStatuses[Fruit](
        Apple -> Remaining,
        Banana -> Remaining,
        Pear -> Remaining,
        Strawberry -> Remaining,
      ),
      candidateVoteCounts = CandidateVoteCounts(
        perCandidate = Map[Fruit, VoteCount](
          Apple -> VoteCount(20, 20),
          Banana -> VoteCount(10, 10),
          Pear -> VoteCount(6, 6),
          Strawberry -> VoteCount(4, 4),
        ),
        exhausted = VoteCount.zero,
        roundingError = VoteCount.zero,
      ),
      transfersDueToIneligibles = Map.empty[Fruit, CandidateVoteCounts[Fruit]],
    ),
    currentDistribution = None,
  )

  private val testPreferenceTree = PreferenceTree.from[Fruit](
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Banana, Pear),
  )

  private val testBundle = AssignedPaperBundle(
    transferValue = 1.0d,
    testPreferenceTree.childFor(Apple).get,
    PaperBundle.Origin.InitialAllocation,
  )

  "a count context" should "derive when a candidate is elected and yet to be distributed" in {
    val mostRecentCountStep = testContext
      .mostRecentCountStep
      .asInstanceOf[AllocationAfterIneligibles[Fruit]]
      .copy(
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Elected(0, 1),
          Banana -> Elected(1, 1),
          Pear -> Remaining,
          Strawberry -> Remaining,
        ),
      )

    val localTestContext = testContext.copy(mostRecentCountStep = mostRecentCountStep)

    assert(localTestContext.electedCandidatesToBeDistributed === Queue(Apple, Banana))
  }

  it should "not indicate a candidate needs to be distributed if they're currently being distributed" in {
    val mostRecentCountStep = testContext
      .mostRecentCountStep
      .asInstanceOf[AllocationAfterIneligibles[Fruit]]
      .copy(
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Elected(0, 1),
          Banana -> Elected(1, 1),
          Pear -> Remaining,
          Strawberry -> Remaining,
        ),
      )

    val localTestContext = testContext.copy(
      mostRecentCountStep = mostRecentCountStep,
      currentDistribution = Some(
        CountContext.CurrentDistribution(
          Apple,
          CandidateDistributionReason.Election,
          Queue(Bag[AssignedPaperBundle[Fruit]](testBundle)),
        )
      )
    )

    assert(localTestContext.electedCandidatesToBeDistributed === Queue(Banana))
  }

  it should "have the right quota" in {
    assert(testContext.quota === QuotaComputation.computeQuota(testContext.numVacancies, testContext.numFormalPapers))
  }

  "a current distribution" can "be for an elected candidate" in {
    val currentDistribution = CountContext.CurrentDistribution(
      Apple,
      CandidateDistributionReason.Election,
      Queue(Bag[AssignedPaperBundle[Fruit]](testBundle))
    )

    assert(currentDistribution.candidateBeingDistributed === Apple)
  }

  it can "be for an excluded candidate" in {
    val currentDistribution = CountContext.CurrentDistribution(
      Apple,
      CandidateDistributionReason.Exclusion,
      Queue(Bag[AssignedPaperBundle[Fruit]](testBundle))
    )

    assert(currentDistribution.candidateBeingDistributed === Apple)
  }

  it must "not have an empty queue of bundles to distribute" in {
    intercept[IllegalArgumentException] {
      CountContext.CurrentDistribution(
        Apple,
        CandidateDistributionReason.Exclusion,
        Queue.empty
      )
    }
  }
}
