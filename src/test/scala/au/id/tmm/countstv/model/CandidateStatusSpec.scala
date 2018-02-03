package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.values.Count
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CandidateStatusSpec extends ImprovedFlatSpec {

  "a candidate status" can "be 'remaining'" in {
    assert(CandidateStatus.Remaining !== null)
  }

  it can "be 'ineligible'" in {
    assert(CandidateStatus.Ineligible !== null)
  }

  it can "be 'elected'" in {
    val elected = CandidateStatus.Elected(ordinalElected = 0, electedAtCount = Count(1))

    assert(elected.ordinalElected === 0)
    assert(elected.electedAtCount === Count(1))
  }

  it can "be 'excluded'" in {
    val excluded = CandidateStatus.Excluded(ordinalExcluded = 0, excludedAtCount = Count(1))

    assert(excluded.ordinalExcluded === 0)
    assert(excluded.excludedAtCount === Count(1))
  }

}
