package au.id.tmm.countstv.counting

object QuotaComputation {

  def computeQuota(numVacancies: Int, numFormalPapers: Long): Long =
    (Math.ceil(numFormalPapers).toLong / (numVacancies + 1)) + 1

}
