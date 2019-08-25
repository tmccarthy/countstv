package au.id.tmm.countstv.model

import org.scalatest.FlatSpec

class VoteCountSpec extends FlatSpec {
  "a vote count" can "be added to another" in {
    assert(VoteCount(1) + VoteCount(2) === VoteCount(3))
  }

  it can "be taken from another" in {
    assert(VoteCount(3) - VoteCount(1) === VoteCount(2))
  }

  "an empty vote count" should "have no votes or papers" in {
    assert(VoteCount.zero === VoteCount(0))
  }
}
