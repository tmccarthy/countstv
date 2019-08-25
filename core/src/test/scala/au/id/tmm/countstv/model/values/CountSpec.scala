package au.id.tmm.countstv.model.values

import org.scalatest.FlatSpec

class CountSpec extends FlatSpec {

  "a count number" can "be incremented" in {
    assert(Count(0).increment === Count(1))
  }

  it can "be decremented" in {
    assert(Count(2).decrement === Count(1))
  }

  it can "be compared to another" in {
    assert(Count(5) > Count(4))
    assert(!(Count(4) > Count(5)))

    assert(Count(5) >= Count(5))
    assert(!(Count(4) >= Count(5)))

    assert(Count(4) <= Count(4))
    assert(!(Count(5) <= Count(4)))

    assert(Count(4) < Count(5))
    assert(!(Count(5) < Count(4)))
  }

  it can "be compared using an ordering" in {
    assert(Count.ordering.compare(Count(6), Count(3)) > 0)
    assert(Count.ordering.compare(Count(3), Count(6)) < 0)
    assert(Count.ordering.compare(Count(3), Count(3)) === 0)
  }

  "the count for the initial allocation" should "be zero" in {
    assert(Count.ofInitialAllocation === Count(0))
  }

  "the count for the allocation away from ineligibles" should "be one" in {
    assert(Count.ofIneligibleCandidateHandling === Count(1))
  }
}
