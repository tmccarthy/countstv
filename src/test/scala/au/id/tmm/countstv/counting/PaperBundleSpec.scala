package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit.{Apple, Banana, Pear, Strawberry}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, PreferenceTree}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class PaperBundleSpec extends ImprovedFlatSpec {

  private val testPreferenceTree = PreferenceTree.from[Fruit](
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Banana, Pear),
  )

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

  private def testDistribution(
                                originalCandidate: Fruit,
                                appleStatus: CandidateStatus = CandidateStatus.Remaining,
                                bananaStatus: CandidateStatus = CandidateStatus.Remaining,
                                pearStatus: CandidateStatus = CandidateStatus.Remaining,
                                strawberryStatus: CandidateStatus = CandidateStatus.Remaining,
                                originalTransferValue: Double = 1d,
                                expectedBundlesAfterDistribution: Set[PaperBundle[Fruit]] = Set.empty,
                                expectBundleUnchanged: Boolean = false,
                              ): Unit = {
    val candidateStatuses = CandidateStatuses[Fruit](
      Fruit.Apple -> appleStatus,
      Fruit.Banana -> bananaStatus,
      Fruit.Pear -> pearStatus,
      Fruit.Strawberry -> strawberryStatus,
    )

    val originalBundle = PaperBundle[Fruit](
      transferValue = originalTransferValue,
      preferenceTreeNode = testPreferenceTree.childFor(originalCandidate).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val origin = PaperBundle.Origin.IneligibleCandidate(originalCandidate)

    val actualBundlesAfterDistribution = originalBundle.distributeToRemainingCandidates(origin, candidateStatuses)

    if (expectBundleUnchanged) {
      assert(actualBundlesAfterDistribution === Set(originalBundle))
    } else {
      assert(actualBundlesAfterDistribution === expectedBundlesAfterDistribution)
    }
  }

  "a paper bundle assigned to a remaining candidate" should "not be distributed" in {
    testDistribution(
      originalCandidate = Banana,
      bananaStatus = CandidateStatus.Remaining,
      expectBundleUnchanged = true
    )
  }

  "a paper bundle assigned to an ineligible candidate" can "be distributed to one bundle" in {
    testDistribution(
      originalCandidate = Banana,
      bananaStatus = CandidateStatus.Ineligible,
      expectedBundlesAfterDistribution = Set(
        PaperBundle[Fruit](
          transferValue = 1d,
          preferenceTreeNode = testPreferenceTree.childFor(Banana, Pear).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Banana),
        )
      )
    )
  }

  it can "be distributed to multiple bundles" in {
    testDistribution(
      originalCandidate = Apple,
      appleStatus = CandidateStatus.Ineligible,
      expectedBundlesAfterDistribution = Set(
        PaperBundle(
          transferValue = 1d,
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
        PaperBundle(
          transferValue = 1d,
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Banana).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
      )
    )
  }

  it can "be distributed to multiple bundles when more than one candidate is ineligible" in {
    testDistribution(
      originalCandidate = Apple,
      appleStatus = CandidateStatus.Ineligible,
      bananaStatus = CandidateStatus.Ineligible,
      expectedBundlesAfterDistribution = Set(
        PaperBundle(
          transferValue = 1d,
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
        PaperBundle(
          transferValue = 1d,
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Banana, Strawberry).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
      )
    )
  }

  it can "be distributed until exhausted" in {
    testDistribution(
      originalCandidate = Banana,
      bananaStatus = CandidateStatus.Excluded(ordinalExcluded = 0, excludedAtCount = 1),
      pearStatus = CandidateStatus.Excluded(ordinalExcluded = 1, excludedAtCount = 2),
      expectedBundlesAfterDistribution = Set.empty,
    )
  }
}
