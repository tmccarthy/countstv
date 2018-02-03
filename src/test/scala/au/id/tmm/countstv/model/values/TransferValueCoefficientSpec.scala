package au.id.tmm.countstv.model.values

import au.id.tmm.utilities.testing.ImprovedFlatSpec

class TransferValueCoefficientSpec extends ImprovedFlatSpec {

  "a transfer coefficient" can "modify a transfer value" in {
    assert(TransferValueCoefficient(0.5d) * TransferValue(0.5d) === TransferValue(0.5d * 0.5d))
  }

  it can "be computed from a surplus and a number of votes" in {
    assert(TransferValueCoefficient.compute(numVotes = NumVotes(160), quota = NumVotes(100)) ===
      TransferValueCoefficient(60d / 160d))
  }

}
