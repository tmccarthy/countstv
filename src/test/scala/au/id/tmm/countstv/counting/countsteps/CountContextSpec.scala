package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.fixtures.{CountContextFixtures, CountFixture}
import au.id.tmm.countstv.counting.{CountAction, QuotaComputation}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CountContextSpec extends ImprovedFlatSpec {

  private val countFixture: CountFixture = CountFixture.withFourCandidates

  private lazy val testContext = countFixture.contextAfterIneligibles

  "a count context" should "have the right quota" in {
    assert(testContext.quota === QuotaComputation.computeQuota(testContext.numVacancies, testContext.numFormalPapers))
  }

  "a terminal count context" should "have no subsequent action" in {
    assert(CountContextFixtures.AfterFinalStep.whereCandidateElectedToRemainingVacancy.nextAction === CountAction.NoAction)
  }
}
