package au.id.tmm.countstv.counting

object QuotaComputation {

  def computeQuota(numVacancies: Int, numBallots: Long): Long = (Math.ceil(numBallots).toLong / (numVacancies + 1)) + 1

}
