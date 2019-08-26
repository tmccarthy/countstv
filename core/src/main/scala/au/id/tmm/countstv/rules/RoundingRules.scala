package au.id.tmm.countstv.rules

final case class RoundingRules(roundQuotaComputation: Boolean, roundTransferValueMultiplication: Boolean)

object RoundingRules {
  val AEC         = RoundingRules(roundQuotaComputation = true, roundTransferValueMultiplication = true)
  val NO_ROUNDING = RoundingRules(roundQuotaComputation = false, roundTransferValueMultiplication = false)
}
