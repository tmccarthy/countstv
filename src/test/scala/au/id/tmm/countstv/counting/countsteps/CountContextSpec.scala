package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.{AssignedPaperBundle, QuotaComputation}
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.CountSteps
import au.id.tmm.countstv.model.values._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.immutable.Queue
import scala.collection.parallel.immutable.ParSet

class CountContextSpec extends ImprovedFlatSpec {

  import CountContextFixture._

  "a count context" should "have the right quota" in {
    assert(testContext.quota === QuotaComputation.computeQuota(testContext.numVacancies, testContext.numFormalPapers))
  }

  it should "have the previous vote counts" in {
    val expectedCandidateVoteCounts = List(
      initialAllocation.candidateVoteCounts,
      allocationAfterIneligibles.candidateVoteCounts,
    )

    assert(testContext.previousCandidateVoteCounts === expectedCandidateVoteCounts)
  }

  it should "indicate if all vacancies are filled" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Elected(Ordinal.first, Count(1)),
      Banana -> Elected(Ordinal.second, Count(1)),
      Pear -> Remaining,
      Strawberry -> Remaining,
    )

    val contextWithAllVacanciesFilled = testContext
      .copy(
        previousCountSteps = CountSteps.AfterIneligibleHandling(
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
      Apple -> Elected(Ordinal.first, Count(1)),
      Banana -> Elected(Ordinal.second, Count(1)),
      Pear -> Elected(Ordinal.third, Count(1)),
      Strawberry -> Ineligible,
    )

    val contextWithAllVacanciesFilled = testContext
      .copy(
        numVacancies = 4,
        previousCountSteps = CountSteps.AfterIneligibleHandling(
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
      Queue(ParSet[AssignedPaperBundle[Fruit]](testBundle)),
      transferValueCoefficient = TransferValueCoefficient(1.0d),
    )

    assert(currentDistribution.candidateBeingDistributed === Apple)
  }

  it can "be for an excluded candidate" in {
    val currentDistribution = CountContext.CurrentDistribution(
      Apple,
      CandidateDistributionReason.Exclusion,
      Queue(ParSet[AssignedPaperBundle[Fruit]](testBundle)),
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
