package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class QuotaComputationSpec extends ImprovedFlatSpec {

  private def testQuota(numVacancies: Int, numBallots: Long, expectedQuota: Long): Unit = {
    s"the quota with $numBallots ballots for $numVacancies vacancies" should s"be $expectedQuota" in {
      assert(QuotaComputation.computeQuota(numVacancies, NumPapers(numBallots)) === NumVotes(expectedQuota))
    }
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
