package au.id.tmm.countstv.model

import au.id.tmm.utilities.testing.ImprovedFlatSpec

class VoteCountSpec extends ImprovedFlatSpec {
  "a vote count" can "be added to another" in {
    assert(VoteCount(1, 1d) + VoteCount(2, 2d) === VoteCount(3, 3d))
  }

  it can "be taken from another" in {
    assert(VoteCount(3, 3d) - VoteCount(1, 1d) === VoteCount(2, 2d))
  }

  it can "be multiplied by a scalar" in {
    assert(VoteCount(2, 3d) * 5 === VoteCount(10, 15d))
  }

  "an empty vote count" should "have no votes or papers" in {
    assert(VoteCount.empty === VoteCount(0, 0d))
  }
}
