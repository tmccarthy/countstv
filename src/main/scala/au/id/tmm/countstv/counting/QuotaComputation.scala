package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}
import au.id.tmm.countstv.rules.RoundingRules

object QuotaComputation {

  def computeQuota(numVacancies: Int, numFormalPapers: NumPapers)(implicit roundingRules: RoundingRules): NumVotes =
    if (roundingRules.roundQuotaComputation) {
      NumVotes((Math.ceil(numFormalPapers.asLong).toLong / (numVacancies + 1)) + 1)
    } else {
      NumVotes((numFormalPapers.asLong.toDouble / (numVacancies.toDouble + 1)) + 1)
    }

}
