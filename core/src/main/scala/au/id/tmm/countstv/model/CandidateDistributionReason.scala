package au.id.tmm.countstv.model

/**
  * A reason for the distribution away from a candidate during a normal distribution step.
  */
sealed trait CandidateDistributionReason

object CandidateDistributionReason {
  case object Election  extends CandidateDistributionReason
  case object Exclusion extends CandidateDistributionReason
}
