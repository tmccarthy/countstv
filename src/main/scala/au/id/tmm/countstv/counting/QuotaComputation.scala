package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}

object QuotaComputation {

  def computeQuota(numVacancies: Int, numFormalPapers: NumPapers): NumVotes =
    NumVotes((Math.ceil(numFormalPapers.asLong).toLong / (numVacancies + 1)) + 1)

}
