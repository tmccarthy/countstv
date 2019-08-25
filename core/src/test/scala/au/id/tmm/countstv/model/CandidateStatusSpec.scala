package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.values.{Count, Ordinal}
import org.scalatest.FlatSpec

class CandidateStatusSpec extends FlatSpec {

  "a candidate status" can "be 'remaining'" in {
    assert(CandidateStatus.Remaining !== null)
  }

  it can "be 'ineligible'" in {
    assert(CandidateStatus.Ineligible !== null)
  }

  it can "be 'elected'" in {
    val elected = CandidateStatus.Elected(Ordinal.first, electedAtCount = Count(1))

    assert(elected.ordinalElected === Ordinal.first)
    assert(elected.electedAtCount === Count(1))
  }

  it can "be 'excluded'" in {
    val excluded = CandidateStatus.Excluded(Ordinal.first, excludedAtCount = Count(1))

    assert(excluded.ordinalExcluded === Ordinal.first)
    assert(excluded.excludedAtCount === Count(1))
  }

}
