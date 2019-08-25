package au.id.tmm.countstv.model

import au.id.tmm.utilities.collection.DupelessSeq

/**
  * A bundle holding in one place the [[CandidateStatus]] of each candidate in a count, with convenience methods.
  */
final case class CandidateStatuses[C](asMap: Map[C, CandidateStatus]) {

  def allCandidates: Set[C] = asMap.keySet

  val electedCandidates: DupelessSeq[C] =
    asMap
      .to(LazyList)
      .collect { case (candidate, status: CandidateStatus.Elected) => candidate -> status }
      .sortBy { case (candidate, status) => status.ordinalElected }
      .map { case (candidate, status) => candidate }
      .to(DupelessSeq)

  val remainingCandidates: Set[C] = candidatesWithStatusMatching(_ == CandidateStatus.Remaining)

  val ineligibleCandidates: Set[C] = candidatesWithStatusMatching(_ == CandidateStatus.Ineligible)

  val excludedCandidates: DupelessSeq[C] =
    asMap
      .toList
      .collect { case (candidate, status: CandidateStatus.Excluded) => candidate -> status }
      .sortBy { case (candidate, status) => status.ordinalExcluded }
      .map { case (candidate, status) => candidate }
      .to(DupelessSeq)

  private def candidatesWithStatusMatching(p: CandidateStatus => Boolean) = asMap
    .to(LazyList)
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

  /**
    * Returns a new object with an updated status for the given candidate.
    */
  def update(candidate: C, newStatus: CandidateStatus): CandidateStatuses[C] =
    CandidateStatuses(asMap.updated(candidate, newStatus))

  /**
    * Returns a new object with updated statuses for the given candidates.
    */
  def updateFrom(newStatuses: Map[C, CandidateStatus]): CandidateStatuses[C] = {
    CandidateStatuses(asMap ++ newStatuses)
  }

  /**
    * Returns a `Map` containing every candidate with a status that differs in the given set of statuses. The values in
    * the map are the new statuses.
    */
  def diff(that: CandidateStatuses[C]): Map[C, CandidateStatus] = {
    this.asMap.flatMap { case (candidate, newStatus) =>
      val oldStatus = that.asMap(candidate)

      if (newStatus != oldStatus) {
        Some(candidate -> newStatus)
      } else {
        None
      }
    }
  }

}

object CandidateStatuses {
  def apply[C](statuses: (C, CandidateStatus)*): CandidateStatuses[C] = CandidateStatuses(statuses.toMap)
}
