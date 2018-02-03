package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.Count
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts}

trait CountStep[C] {

  def count: Count

  def candidateStatuses: CandidateStatuses[C]

  def candidateVoteCounts: CandidateVoteCounts[C]

}
