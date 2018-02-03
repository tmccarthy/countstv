package au.id.tmm.countstv.model

sealed trait CandidateDistributionReason

object CandidateDistributionReason {
  case object Election extends CandidateDistributionReason
  case object Exclusion extends CandidateDistributionReason
}
