package au.id.tmm.countstv.model

import au.id.tmm.countstv.Count

sealed trait CandidateStatus

object CandidateStatus {
  case object Remaining extends CandidateStatus
  case object Ineligible extends CandidateStatus
  case class Elected(ordinalElected: Int, electedAtCount: Count) extends CandidateStatus
  case class Excluded(ordinalExcluded: Int, excludedAtCount: Count) extends CandidateStatus
}
