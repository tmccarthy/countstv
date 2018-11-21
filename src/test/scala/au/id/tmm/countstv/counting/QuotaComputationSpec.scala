package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.values.NumPapers
import au.id.tmm.countstv.rules.RoundingRules
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class QuotaComputationSpec extends ImprovedFlatSpec {

  private def testQuota(numVacancies: Int, numBallots: Long, expectedQuota: Double, round: Boolean): Unit = {
    s"the ${if (round) "rounded" else "unrounded"} quota with $numBallots ballots for $numVacancies vacancies" should s"be $expectedQuota" in {
      val actualQuota = QuotaComputation.computeQuota(numVacancies, NumPapers(numBallots))(RoundingRules.AEC.copy(roundQuotaComputation = round))

      assert(actualQuota.asDouble === expectedQuota)
    }
  }

  testQuota(
    numVacancies = 12,
    numBallots = 1061165,
    expectedQuota = 81629.07692307692,
    round = false,
  )

  testQuota(
    numVacancies = 12,
    numBallots = 1061165,
    expectedQuota = 81629,
    round = true,
  )

  testQuota(
    numVacancies = 12,
    numBallots = 1366182,
    expectedQuota = 105091.92307692308,
    round = false,
  )

  testQuota(
    numVacancies = 12,
    numBallots = 1366182,
    expectedQuota = 105091,
    round = true,
  )

  testQuota(
    numVacancies = 2,
    numBallots = 254767,
    expectedQuota = 84923.33333333333,
    round = false,
  )

  testQuota(
    numVacancies = 2,
    numBallots = 254767,
    expectedQuota = 84923,
    round = true,
  )

}
