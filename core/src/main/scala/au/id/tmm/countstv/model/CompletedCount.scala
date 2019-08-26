package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.countsteps.CountSteps
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}

final case class CompletedCount[C](
  countParams: CountParams[C],
  numFormalPapers: NumPapers,
  quota: NumVotes,
  countSteps: CountSteps.AllowingAppending[C],
) {
  def outcomes: CandidateStatuses[C] = countSteps.last.candidateStatuses
}
