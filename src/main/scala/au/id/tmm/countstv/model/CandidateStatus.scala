package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.values.{Count, Ordinal}

/**
  * The status of a candidate during a count.
  */
sealed trait CandidateStatus

object CandidateStatus {

  /**
    * The status of candidates that remain in the count.
    */
  case object Remaining extends CandidateStatus

  /**
    * The status of candidates that were ineligible for inclusion in the count.
    */
  case object Ineligible extends CandidateStatus

  /**
    * The status of candidates that have been elected in the count.
    * @param ordinalElected whether this candidate was elected first, second etc.
    * @param electedAtCount the count at which the candidate was elected
    */
  case class Elected(ordinalElected: Ordinal, electedAtCount: Count) extends CandidateStatus


  /**
    * The status of candidates that have been excluded in the count.
    * @param ordinalExcluded whether this candidate was excluded first, second etc.
    * @param excludedAtCount the count at which the candidate was excluded
    */
  case class Excluded(ordinalExcluded: Ordinal, excludedAtCount: Count) extends CandidateStatus

}
