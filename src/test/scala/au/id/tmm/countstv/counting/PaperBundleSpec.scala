package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit.{Apple, Banana, Pear, Strawberry}
import au.id.tmm.countstv.model.PreferenceTree
import au.id.tmm.countstv.{BallotFixtures, Fruit}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class PaperBundleSpec extends ImprovedFlatSpec {

  private val testPreferenceTree = PreferenceTree.from(List(BallotFixtures.ballotWith4Preferences))
  private val bananaPreferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get

  "a paper bundle" should "have a transfer value" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = bananaPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.transferValue === 0.5d)
  }

  it should "refer to a PreferenceTreeNode" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = bananaPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.preferenceTreeNode eq bananaPreferenceTreeNode)
  }

  it should "have an origin" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = bananaPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.origin === PaperBundle.Origin.InitialAllocation)
  }

  "a paper bundle's origin" can "be the initial allocation" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = bananaPreferenceTreeNode,
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
      preferenceTreeNode = bananaPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.associatedCandidate === Fruit.Banana)
  }

  "a paper bundle not assigned to an ineligible candidate" should "not be distributed" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 1d,
      preferenceTreeNode = bananaPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val ineligibleCandidates: Set[Fruit] = Set(Fruit.Apple)

    val bundlesAfterDistribution = paperBundle.distributionGivenIneligibles(ineligibleCandidates)

    assert(bundlesAfterDistribution === Set(paperBundle))
  }

  "a paper bundle assigned to an ineligible candidate" can "be distributed to one bundle" in {
    val originalBundle = PaperBundle[Fruit](
      transferValue = 1d,
      preferenceTreeNode = bananaPreferenceTreeNode,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val ineligibleCandidates: Set[Fruit] = Set(Fruit.Banana)

    val bundlesAfterDistribution = originalBundle.distributionGivenIneligibles(ineligibleCandidates)

    val expectedBundle = PaperBundle[Fruit](
      transferValue = 1d,
      preferenceTreeNode = bananaPreferenceTreeNode.childFor(Fruit.Pear).get,
      origin = PaperBundle.Origin.IneligibleCandidate(Fruit.Banana),
    )

    assert(bundlesAfterDistribution === Set(expectedBundle))
  }

  it can "be distributed to multiple bundles" in {
    val preferenceTree = PreferenceTree.from[Fruit](
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    val originalBundle = PaperBundle(
      transferValue = 1d,
      preferenceTreeNode = preferenceTree.childFor(Fruit.Apple).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val ineligibleCandidates: Set[Fruit] = Set(Fruit.Apple)

    val actualBundlesAfterDistribution = originalBundle.distributionGivenIneligibles(ineligibleCandidates)

    val expectedBundlesAfterDistribution = Set(
      PaperBundle(
        transferValue = 1d,
        preferenceTreeNode = preferenceTree.childFor(Apple, Pear).get,
        origin = PaperBundle.Origin.IneligibleCandidate(Apple),
      ),
      PaperBundle(
        transferValue = 1d,
        preferenceTreeNode = preferenceTree.childFor(Apple, Banana).get,
        origin = PaperBundle.Origin.IneligibleCandidate(Apple),
      ),
    )

    assert(actualBundlesAfterDistribution === expectedBundlesAfterDistribution)
  }

  it can "be distributed to multiple bundles when more than one candidate is ineligible" in {
    val preferenceTree = PreferenceTree.from[Fruit](
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    val originalBundle = PaperBundle(
      transferValue = 1d,
      preferenceTreeNode = preferenceTree.childFor(Apple).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val ineligibleCandidates: Set[Fruit] = Set(Apple, Banana)

    val actualBundlesAfterDistribution = originalBundle.distributionGivenIneligibles(ineligibleCandidates)

    val expectedBundlesAfterDistribution = Set(
      PaperBundle(
        transferValue = 1d,
        preferenceTreeNode = preferenceTree.childFor(Apple, Pear).get,
        origin = PaperBundle.Origin.IneligibleCandidate(Apple),
      ),
      PaperBundle(
        transferValue = 1d,
        preferenceTreeNode = preferenceTree.childFor(Apple, Banana, Strawberry).get,
        origin = PaperBundle.Origin.IneligibleCandidate(Apple),
      ),
    )

    assert(actualBundlesAfterDistribution === expectedBundlesAfterDistribution)
  }

}
