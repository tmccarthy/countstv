package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.values.{Count, Ordinal}

sealed trait CandidateStatus

object CandidateStatus {
  case object Remaining extends CandidateStatus
  case object Ineligible extends CandidateStatus
  case class Elected(ordinalElected: Ordinal, electedAtCount: Count) extends CandidateStatus
  case class Excluded(ordinalExcluded: Ordinal, excludedAtCount: Count) extends CandidateStatus
}
