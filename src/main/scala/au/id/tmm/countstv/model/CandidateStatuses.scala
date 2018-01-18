package au.id.tmm.countstv.model

final case class CandidateStatuses[C](asMap: Map[C, CandidateStatus]) {
  def allCandidates: Set[C] = asMap.keySet

  val elected: Set[C] = candidatesWithStatusMatching {
    case _: CandidateStatus.Elected => true
    case _ => false
  }

  val remaining: Set[C] = candidatesWithStatusMatching(_ == CandidateStatus.Remaining)

  val ineligible: Set[C] = candidatesWithStatusMatching(_ == CandidateStatus.Ineligible)

  val excluded: Set[C] = candidatesWithStatusMatching {
    case _: CandidateStatus.Excluded => true
    case _ => false
  }

  private def candidatesWithStatusMatching(p: CandidateStatus => Boolean) = asMap
    .toStream
    .collect {
      case (candidate, status) if p(status) => candidate
    }
    .toSet

  val ineligibleForPreferenceFlows: Set[C] = allCandidates -- remaining

}

object CandidateStatuses {
  def apply[C](statuses: (C, CandidateStatus)*): CandidateStatuses[C] = CandidateStatuses(statuses.toMap)
}
