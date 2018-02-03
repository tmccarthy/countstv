package au.id.tmm.countstv.model.values

import au.id.tmm.utilities.testing.ImprovedFlatSpec

class NumVotesSpec extends ImprovedFlatSpec {

  "a number of votes" can "be rounded down" in {
    assert(NumVotes(42.42d).roundedDown === NumVotes(42.0d))
  }

  it can "be added to another" in {
    assert(NumVotes(2) + NumVotes(3) === NumVotes(5))
  }

  it can "be subtracted from another" in {
    assert(NumVotes(5) - NumVotes(2) === NumVotes(3))
  }

  it can "be compared to another" in {
    assert(NumVotes(5) > NumVotes(4))
    assert(!(NumVotes(4) > NumVotes(5)))

    assert(NumVotes(5) >= NumVotes(5))
    assert(!(NumVotes(4) >= NumVotes(5)))

    assert(NumVotes(4) <= NumVotes(4))
    assert(!(NumVotes(5) <= NumVotes(4)))

    assert(NumVotes(4) < NumVotes(5))
    assert(!(NumVotes(5) < NumVotes(4)))
  }

  it can "be compared using an ordering" in {
    assert(NumVotes.ordering.compare(NumVotes(6), NumVotes(3)) > 0)
    assert(NumVotes.ordering.compare(NumVotes(3), NumVotes(6)) < 0)
    assert(NumVotes.ordering.compare(NumVotes(3), NumVotes(3)) === 0)
  }
}
