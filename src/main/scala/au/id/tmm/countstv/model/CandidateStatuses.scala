package au.id.tmm.countstv.model

import au.id.tmm.utilities.collection.DupelessSeq

/**
  * A bundle holding in one place the [[CandidateStatus]] of each candidate in a count, with convenience methods.
  */
final case class CandidateStatuses[C](asMap: Map[C, CandidateStatus]) {

  def allCandidates: Set[C] = asMap.keySet

  val electedCandidates: DupelessSeq[C] =
    asMap
      .toStream
      .collect { case (candidate, status: CandidateStatus.Elected) => candidate -> status }
      .sortBy { case (candidate, status) => status.ordinalElected }
      .map { case (candidate, status) => candidate }
      .to[DupelessSeq]

  val remainingCandidates: Set[C] = candidatesWithStatusMatching(_ == CandidateStatus.Remaining)

  val ineligibleCandidates: Set[C] = candidatesWithStatusMatching(_ == CandidateStatus.Ineligible)

  val excludedCandidates: Set[C] = candidatesWithStatusMatching {
    case _: CandidateStatus.Excluded => true
    case _ => false
  }

  private def candidatesWithStatusMatching(p: CandidateStatus => Boolean) = asMap
    .toStream
    .collect {
      case (candidate, status) if p(status) => candidate
    }
    .toSet

  /**
    * Candidates that are not remaining.
    */
  val ineligibleForPreferenceFlows: Set[C] = allCandidates -- remainingCandidates

  /**
    * Candidates that are not ineligible.
    */
  val eligibleCandidates: Set[C] = allCandidates -- ineligibleCandidates

  def update(candidate: C, newStatus: CandidateStatus): CandidateStatuses[C] =
    CandidateStatuses(asMap.updated(candidate, newStatus))

  def updateFrom(newStatuses: Map[C, CandidateStatus]): CandidateStatuses[C] = {
    CandidateStatuses(asMap ++ newStatuses)
  }

}

object CandidateStatuses {
  def apply[C](statuses: (C, CandidateStatus)*): CandidateStatuses[C] = CandidateStatuses(statuses.toMap)
}
