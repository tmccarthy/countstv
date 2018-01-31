package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.immutable.{Bag, HashedBagConfiguration, Queue}

class CountContextSpec extends ImprovedFlatSpec {

  private implicit val bagConfiguration: HashedBagConfiguration[PaperBundle[Fruit]] = PaperBundle.bagConfiguration

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
    currentDistribution = CountContext.CurrentDistribution.NoDistribution,
    paperBundlesToBeDistributed = Bag.empty[PaperBundle[Fruit]],
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
      currentDistribution = CountContext.CurrentDistribution.ElectedCandidate(Apple),
    )

    assert(localTestContext.electedCandidatesToBeDistributed === Queue(Banana))
  }

  it should "have the right quota" in {
    assert(testContext.quota === QuotaComputation.computeQuota(testContext.numVacancies, testContext.numFormalPapers))
  }

  "it" can "be currently distributing an excluded candidate" in {
    val localTestContext = testContext
      .copy(currentDistribution = CountContext.CurrentDistribution.ExcludedCandidate(Apple))

    assert(localTestContext.currentDistribution === CountContext.CurrentDistribution.ExcludedCandidate(Apple))
  }
}
