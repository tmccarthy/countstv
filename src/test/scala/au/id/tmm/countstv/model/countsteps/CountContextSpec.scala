package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.values._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.immutable.{Bag, HashedBagConfiguration, Queue}

class CountContextSpec extends ImprovedFlatSpec {

  private implicit def bagConfiguration[A]: HashedBagConfiguration[A] =
    PaperBundle.bagConfiguration.asInstanceOf[HashedBagConfiguration[A]]

  private val initialAllocation = InitialAllocation[Fruit](
    candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Remaining,
      Banana -> Remaining,
      Pear -> Remaining,
      Strawberry -> Remaining,
    ),
    candidateVoteCounts = CandidateVoteCounts(
      perCandidate = Map[Fruit, VoteCount](
        Apple -> VoteCount(20),
        Banana -> VoteCount(10),
        Pear -> VoteCount(6),
        Strawberry -> VoteCount(4),
      ),
      exhausted = VoteCount.zero,
      roundingError = VoteCount.zero,
    ),
  )

  private val allocationAfterIneligibles = AllocationAfterIneligibles[Fruit](
    candidateStatuses = initialAllocation.candidateStatuses,
    candidateVoteCounts = initialAllocation.candidateVoteCounts,
    transfersDueToIneligibles = Map.empty[Fruit, CandidateVoteCounts[Fruit]],
  )

  private val testContext = CountContext(
    numFormalPapers = NumPapers(40),
    numVacancies = 2,
    paperBundles = Bag.empty[PaperBundle[Fruit]],
    previousCountSteps = List(initialAllocation, allocationAfterIneligibles),
    currentDistribution = None,
  )

  private val testPreferenceTree = PreferenceTree.from[Fruit](
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Banana, Pear),
  )

  private val testBundle = AssignedPaperBundle(
    transferValue = TransferValue(1.0d),
    testPreferenceTree.childFor(Apple).get,
    PaperBundle.Origin.InitialAllocation,
  )

  "a count context" should "derive when a candidate is elected and yet to be distributed" in {
    val mostRecentCountStep = testContext
      .mostRecentCountStep
      .asInstanceOf[AllocationAfterIneligibles[Fruit]]
      .copy(
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Elected(0, Count(1)),
          Banana -> Elected(1, Count(1)),
          Pear -> Remaining,
          Strawberry -> Remaining,
        ),
      )

    val localTestContext = testContext.copy(previousCountSteps = List(mostRecentCountStep))

    assert(localTestContext.electedCandidatesWaitingToBeDistributed === Queue(Apple, Banana))
  }

  it should "not indicate a candidate needs to be distributed if they're currently being distributed" in {
    val mostRecentCountStep = testContext
      .mostRecentCountStep
      .asInstanceOf[AllocationAfterIneligibles[Fruit]]
      .copy(
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Elected(0, Count(1)),
          Banana -> Elected(1, Count(1)),
          Pear -> Remaining,
          Strawberry -> Remaining,
        ),
      )

    val localTestContext = testContext.copy(
      previousCountSteps = List(mostRecentCountStep),
      currentDistribution = Some(
        CountContext.CurrentDistribution(
          Apple,
          CandidateDistributionReason.Election,
          Queue(Bag[AssignedPaperBundle[Fruit]](testBundle)),
          transferValueCoefficient = TransferValueCoefficient(1.0d),
        )
      )
    )

    assert(localTestContext.electedCandidatesWaitingToBeDistributed === Queue(Banana))
  }

  it should "not indicate a candidate needs to be distributed if they've already been distributed" in {
    val mostRecentCountStep = testContext
      .mostRecentCountStep
      .asInstanceOf[AllocationAfterIneligibles[Fruit]]
      .copy(
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> Elected(0, Count(1)),
          Banana -> Remaining,
          Pear -> Remaining,
          Strawberry -> Remaining,
        ),
        candidateVoteCounts = CandidateVoteCounts[Fruit](
          perCandidate = Map[Fruit, VoteCount](
            Apple -> VoteCount(NumPapers(0), NumVotes(14)),
            Banana -> VoteCount(NumPapers(12), NumVotes(10)),
            Pear -> VoteCount(NumPapers(16), NumVotes(6)),
            Strawberry -> VoteCount(NumPapers(12), NumVotes(10)),
          ),
          exhausted = VoteCount.zero,
          roundingError = VoteCount.zero,
        )
      )

    val localTestContext = testContext.copy(previousCountSteps = List(mostRecentCountStep))

    assert(localTestContext.electedCandidatesWaitingToBeDistributed === Queue())
  }

  it should "have the right quota" in {
    assert(testContext.quota === QuotaComputation.computeQuota(testContext.numVacancies, testContext.numFormalPapers))
  }

  it should "have the previous vote counts" in {
    val expectedCandidateVoteCounts = List(
      initialAllocation.candidateVoteCounts,
      allocationAfterIneligibles.candidateVoteCounts,
    )

    assert(testContext.previousCandidateVoteCounts === expectedCandidateVoteCounts)
  }

  it can "not have no previous count steps" in {
    intercept[IllegalArgumentException] {
      testContext.copy(previousCountSteps = List.empty)
    }
  }

  it can "not have no previous count steps when using the convenience constructor" in {
    intercept[IllegalArgumentException] {
      CountContext(
        numFormalPapers = NumPapers(40),
        numVacancies = 2,
        paperBundles = Bag.empty[PaperBundle[Fruit]],
        previousCountSteps = List.empty,
        currentDistribution = None,
      )
    }
  }

  it should "indicate if all vacancies are filled" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Elected(0, Count(1)),
      Banana -> Elected(1, Count(1)),
      Pear -> Remaining,
      Strawberry -> Remaining,
    )

    val contextWithAllVacanciesFilled = testContext
      .copy(
        previousCountSteps = List(
          initialAllocation,
          allocationAfterIneligibles.copy(
            candidateStatuses = candidateStatuses,
          ),
        ),
        candidateStatuses = candidateStatuses,
      )

    assert(contextWithAllVacanciesFilled.allVacanciesNowFilled === true)
  }

  it should "indicate if all vacancies are not filled" in {
    assert(testContext.allVacanciesNowFilled === false)
  }

  it should "indicate that all vacancies are filled if all candidates have been elected even if there are " +
    "unfilled vacancies" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Elected(0, Count(1)),
      Banana -> Elected(1, Count(1)),
      Pear -> Elected(2, Count(1)),
      Strawberry -> Elected(3, Count(1)),
    )

    val contextWithAllVacanciesFilled = testContext
      .copy(
        numVacancies = 5,
        previousCountSteps = List(
          initialAllocation,
          allocationAfterIneligibles.copy(
            candidateStatuses = candidateStatuses,
          ),
        ),
        candidateStatuses = candidateStatuses,
      )

    assert(contextWithAllVacanciesFilled.allVacanciesNowFilled === true)
  }

  "a current distribution" can "be for an elected candidate" in {
    val currentDistribution = CountContext.CurrentDistribution(
      Apple,
      CandidateDistributionReason.Election,
      Queue(Bag[AssignedPaperBundle[Fruit]](testBundle)),
      transferValueCoefficient = TransferValueCoefficient(1.0d),
    )

    assert(currentDistribution.candidateBeingDistributed === Apple)
  }

  it can "be for an excluded candidate" in {
    val currentDistribution = CountContext.CurrentDistribution(
      Apple,
      CandidateDistributionReason.Exclusion,
      Queue(Bag[AssignedPaperBundle[Fruit]](testBundle)),
      transferValueCoefficient = TransferValueCoefficient(1.0d),
    )

    assert(currentDistribution.candidateBeingDistributed === Apple)
  }

  it must "not have an empty queue of bundles to distribute" in {
    intercept[IllegalArgumentException] {
      CountContext.CurrentDistribution(
        Apple,
        CandidateDistributionReason.Exclusion,
        Queue.empty,
        transferValueCoefficient = TransferValueCoefficient(1.0d),
      )
    }
  }
}
