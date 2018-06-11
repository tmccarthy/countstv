package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.CandidateDistributionReason

private[counting] sealed trait CountAction[+C]

private[counting] object CountAction {

  case object AllocateAwayFromIneligibles
    extends CountAction[Nothing]
      with Actionable[Nothing]

  sealed trait Actionable[+C] extends CountAction[C]
  sealed trait DuringDistribution[+C] extends CountAction[C]
  sealed trait ActionableDuringDistribution[+C] extends CountAction[C]
    with Actionable[C]
    with DuringDistribution[C]

  case class DistributeFromCandidate[C](candidate: C, reason: CandidateDistributionReason)
    extends CountAction[C]
      with ActionableDuringDistribution[C]

  case object ElectAllRemainingCandidates
    extends CountAction[Nothing]
      with ActionableDuringDistribution[Nothing]

  case class MarkCandidateFinallyElected[C](candidate: C)
    extends CountAction[C]
      with ActionableDuringDistribution[C]

  case object NoAction
    extends CountAction[Nothing]
      with DuringDistribution[Nothing]

}
