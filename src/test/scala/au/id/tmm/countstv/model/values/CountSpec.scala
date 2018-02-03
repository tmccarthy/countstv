package au.id.tmm.countstv.model.values

import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CountSpec extends ImprovedFlatSpec {

  "a count number" can "be incremented" in {
    assert(Count(0).increment === Count(1))
  }

  "the count for the initial allocation" should "be zero" in {
    assert(Count.ofInitialAllocation === Count(0))
  }

  "the count for the allocation away from ineligibles" should "be one" in {
    assert(Count.ofIneligibleCandidateHandling === Count(1))
  }
}
