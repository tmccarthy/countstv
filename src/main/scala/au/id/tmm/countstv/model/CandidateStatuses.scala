package au.id.tmm.countstv.model

import au.id.tmm.utilities.collection.OrderedSet

final case class CandidateStatuses[C](asMap: Map[C, CandidateStatus]) {
  def allCandidates: Set[C] = asMap.keySet

  val electedCandidates: OrderedSet[C] =
    asMap
      .toStream
      .collect { case (candidate, status: CandidateStatus.Elected) => candidate -> status }
      .sortBy { case (candidate, status) => status.ordinalElected }
      .map { case (candidate, status) => candidate }
      .to[OrderedSet]

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

  val ineligibleForPreferenceFlows: Set[C] = allCandidates -- remainingCandidates

}

object CandidateStatuses {
  def apply[C](statuses: (C, CandidateStatus)*): CandidateStatuses[C] = CandidateStatuses(statuses.toMap)
}
