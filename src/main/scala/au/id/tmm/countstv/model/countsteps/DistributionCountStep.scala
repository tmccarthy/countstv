package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.Count
import au.id.tmm.countstv.model.{CandidateDistributionReason, CandidateStatuses, CandidateVoteCounts}

final case class DistributionCountStep[C] (
                                            count: Count,
                                            candidateStatuses: CandidateStatuses[C],
                                            candidateVoteCounts: CandidateVoteCounts[C],
                                            distributionSource: DistributionCountStep.Source[C],
                                          ) extends CountStep[C] {

}

object DistributionCountStep {

  final case class Source[C](
                              candidate: C,
                              candidateDistributionReason: CandidateDistributionReason,
                              sourceCounts: Set[Count],
                              transferValue: Double,
                            )

}