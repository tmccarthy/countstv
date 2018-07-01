package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.countsteps.CountSteps
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}

final case class CompletedCount[C](
                                    numVacancies: Int,
                                    numFormalPapers: NumPapers,
                                    quota: NumVotes,

                                    // TODO what about when count is completed after ineligible handling
                                    countSteps: CountSteps.DistributionPhase[C],
                                  ) {
  def outcomes: CandidateStatuses[C] = countSteps.last.candidateStatuses
}
