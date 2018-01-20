package au.id.tmm.countstv.counting

import au.id.tmm.utilities.testing.ImprovedFlatSpec

class QuotaComputationSpec extends ImprovedFlatSpec {

  private def testQuota(numVacancies: Int, numBallots: Long, expectedQuota: Long): Unit = {
    assert(QuotaComputation.computeQuota(numVacancies, numBallots) === expectedQuota)
  }

  testQuota(
    numVacancies = 12,
    numBallots = 1061165,
    expectedQuota = 81629,
  )

  testQuota(
    numVacancies = 12,
    numBallots = 1366182,
    expectedQuota = 105091,
  )

  testQuota(
    numVacancies = 2,
    numBallots = 254767,
    expectedQuota = 84923,
  )

}
