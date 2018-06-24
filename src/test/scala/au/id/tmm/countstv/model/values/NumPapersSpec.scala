package au.id.tmm.countstv.model.values

import au.id.tmm.utilities.testing.ImprovedFlatSpec

class NumPapersSpec extends ImprovedFlatSpec {

  "a number of papers" can "be added to another" in {
    assert(NumPapers(2) + NumPapers(5) === NumPapers(7))
  }

  it can "be subtracted from another" in {
    assert(NumPapers(5) - NumPapers(2) === NumPapers(3))
  }

  it can "be compared to another" in {
    assert(NumPapers(5) > NumPapers(4))
    assert(!(NumPapers(4) > NumPapers(5)))

    assert(NumPapers(5) >= NumPapers(5))
    assert(!(NumPapers(4) >= NumPapers(5)))

    assert(NumPapers(4) <= NumPapers(4))
    assert(!(NumPapers(5) <= NumPapers(4)))

    assert(NumPapers(4) < NumPapers(5))
    assert(!(NumPapers(5) < NumPapers(4)))
  }

  it can "be compared using an ordering" in {
    assert(NumPapers.ordering.compare(NumPapers(6), NumPapers(3)) > 0)
    assert(NumPapers.ordering.compare(NumPapers(3), NumPapers(6)) < 0)
    assert(NumPapers.ordering.compare(NumPapers(3), NumPapers(3)) === 0)
  }

  it can "be multiplied by a transferValue" in {
    assert(NumPapers(5) * TransferValue(0.75) === NumVotes(3))
  }

}
