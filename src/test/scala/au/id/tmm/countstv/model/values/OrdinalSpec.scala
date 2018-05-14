package au.id.tmm.countstv.model.values

import au.id.tmm.utilities.testing.ImprovedFlatSpec

class OrdinalSpec extends ImprovedFlatSpec {

  "an ordinal" can "be compared to another" in {
    assert(Ordinal(5) > Ordinal(4))
    assert(!(Ordinal(4) > Ordinal(5)))

    assert(Ordinal(5) >= Ordinal(5))
    assert(!(Ordinal(4) >= Ordinal(5)))

    assert(Ordinal(4) <= Ordinal(4))
    assert(!(Ordinal(5) <= Ordinal(4)))

    assert(Ordinal(4) < Ordinal(5))
    assert(!(Ordinal(5) < Ordinal(4)))
  }

  it can "be compared using an ordering" in {
    assert(Ordinal.ordering.compare(Ordinal(6), Ordinal(3)) > 0)
    assert(Ordinal.ordering.compare(Ordinal(3), Ordinal(6)) < 0)
    assert(Ordinal.ordering.compare(Ordinal(3), Ordinal(3)) === 0)
  }

  it can "be first" in {
    assert(Ordinal.first === Ordinal(0))
  }

  it can "be second" in {
    assert(Ordinal.second === Ordinal(1))
  }

  it can "be third" in {
    assert(Ordinal.third === Ordinal(2))
  }

  it can "be fourth" in {
    assert(Ordinal.fourth === Ordinal(3))
  }

  it can "be fifth" in {
    assert(Ordinal.fifth === Ordinal(4))
  }

  it can "be sixth" in {
    assert(Ordinal.sixth === Ordinal(5))
  }

  it can "be seventh" in {
    assert(Ordinal.seventh === Ordinal(6))
  }

  it can "be eighth" in {
    assert(Ordinal.eighth === Ordinal(7))
  }

  it can "be ninth" in {
    assert(Ordinal.ninth === Ordinal(8))
  }

  it can "be tenth" in {
    assert(Ordinal.tenth === Ordinal(9))
  }

  it can "be constructed for the next element in a collection" in {
    val list = List(1, 2, 3, 4, 5)

    assert(Ordinal.ofNextAdditionTo(list) === Ordinal.sixth)
  }

}
