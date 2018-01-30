package au.id.tmm.countstv.model

import au.id.tmm.countstv.Count

trait CountStep[C] {

  def count: Count

  def candidateStatuses: CandidateStatuses[C]

  def candidateVoteCounts: CandidateVoteCounts[C]

}
