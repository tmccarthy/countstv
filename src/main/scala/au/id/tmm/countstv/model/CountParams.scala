package au.id.tmm.countstv.model

import au.id.tmm.countstv.rules.RoundingRules

final case class CountParams[C](
                                 candidates: Set[C],
                                 ineligibleCandidates: Set[C],
                                 numVacancies: Int,
                                 roundingRules: RoundingRules,
                               )
