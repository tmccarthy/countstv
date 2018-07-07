package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.counting.fixtures.CountFixture
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CountContextSpec extends ImprovedFlatSpec {

  private val countFixture: CountFixture = CountFixture.withFourCandidates

  private lazy val testContext = countFixture.contextAfterIneligibles

  "a count context" should "have the right quota" in {
    assert(testContext.quota === QuotaComputation.computeQuota(testContext.numVacancies, testContext.numFormalPapers))
  }
}
