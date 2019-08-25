package au.id.tmm.countstv.model.values

import org.scalatest.FlatSpec

class TransferValueSpec extends FlatSpec {

  "a transfer value" can "be compared to another" in {
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
