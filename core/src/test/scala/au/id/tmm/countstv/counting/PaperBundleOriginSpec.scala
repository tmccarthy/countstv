package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit.{Apple, Banana, Pear, Strawberry}
import au.id.tmm.countstv.model.preferences.PreferenceTree
import au.id.tmm.countstv.model.values.{Count, TransferValue}
import org.scalatest.FlatSpec

class PaperBundleOriginSpec extends FlatSpec {

  private val testPreferenceTree = PreferenceTree.from[Fruit](Set(Apple, Pear, Banana, Strawberry), numBallotsHint = 3)(
    List(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    ))

  "a paper bundle's origin" can "be the initial allocation" in {
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = TransferValue(0.5d),
      preferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.origin === PaperBundle.Origin.InitialAllocation)
  }

  it can "be distribution from an ineligible candidate" in {
    val ineligibleCandidate = Fruit.Apple

    val origin = PaperBundle.Origin.IneligibleCandidate[Fruit](ineligibleCandidate)

    assert(origin.source === ineligibleCandidate)
  }

  it can "be distribution from an elected candidate" in {
    val electedCandidate = Fruit.Apple
    val transferValue    = TransferValue(0.7d)
    val count            = Count(4)

    val origin = PaperBundle.Origin.ElectedCandidate[Fruit](electedCandidate, transferValue, count)

    assert(origin.source === electedCandidate)
    assert(origin.transferValue === transferValue)
    assert(origin.count === count)
  }

  it can "be distribution from an excluded candidate" in {
    val excludedCandidate = Fruit.Apple
    val count             = Count(4)

    val origin = PaperBundle.Origin.ExcludedCandidate[Fruit](excludedCandidate, count)

    assert(origin.source === excludedCandidate)
    assert(origin.count === count)
  }

  "a paper bundle originating at the initial allocation" should "originate from the count 0" in {
    assert(PaperBundle.Origin.InitialAllocation.count === Count(0))
  }

  "a paper bundle originating after ineligible candidates have been handled" should "originate from the count 1" in {
    assert(PaperBundle.Origin.IneligibleCandidate(Apple).count === Count(1))
  }

}
