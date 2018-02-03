package au.id.tmm.countstv.model.values

import au.id.tmm.utilities.testing.ImprovedFlatSpec

class TransferValueSpec extends ImprovedFlatSpec {

  "a transfer value" can "be multipled by a number of ballots to get a number of votes" in {
    assert(TransferValue(0.7d) * NumPapers(100) === NumVotes(70d))
  }

  it can "be compared to another" in {
    assert(TransferValue(0.5) > TransferValue(0.4))
    assert(!(TransferValue(0.4) > TransferValue(0.5)))

    assert(TransferValue(0.5) >= TransferValue(0.5))
    assert(!(TransferValue(0.4) >= TransferValue(0.5)))

    assert(TransferValue(0.4) <= TransferValue(0.4))
    assert(!(TransferValue(0.5) <= TransferValue(0.4)))

    assert(TransferValue(0.4) < TransferValue(0.5))
    assert(!(TransferValue(0.5) < TransferValue(0.4)))
  }

  it can "be compared using an ordering" in {
    assert(TransferValue.ordering.compare(TransferValue(0.6), TransferValue(0.3)) > 0)
    assert(TransferValue.ordering.compare(TransferValue(0.3), TransferValue(0.6)) < 0)
    assert(TransferValue.ordering.compare(TransferValue(0.3), TransferValue(0.3)) === 0)
  }

}
