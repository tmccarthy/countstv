package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.PreferenceTree
import au.id.tmm.countstv.{BallotFixtures, Fruit}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class PaperBundleSpec extends ImprovedFlatSpec {

  private val testPreferenceTree = PreferenceTree.from(List(BallotFixtures.ballotWith4Preferences))
  private val testPreferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get

  "a paper bundle" should "have a transfer value" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = testPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.transferValue === 0.5d)
  }

  it should "refer to a PreferenceTreeNode" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = testPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.preferenceTreeNode eq testPreferenceTreeNode)
  }

  it should "have an origin" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = testPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.origin === PaperBundle.Origin.InitialAllocation)
  }

  "a paper bundle's origin" can "be the initial allocation" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = testPreferenceTreeNode,
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
    val transferValue = 0.7d

    val origin = PaperBundle.Origin.ElectedCandidate[Fruit](electedCandidate, transferValue)

    assert(origin.source === electedCandidate)
    assert(origin.transferValue === transferValue)
  }

  it can "be distribution from an excluded candidate" in {
    val excludedCandidate = Fruit.Apple

    val origin = PaperBundle.Origin.ExcludedCandidate[Fruit](excludedCandidate)

    assert(origin.source === excludedCandidate)
  }

  "a paper bundle" should "be assigned to a candidate" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = testPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.associatedCandidate === Fruit.Banana)
  }

}
