package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit.{Apple, Banana, Pear, Strawberry}
import au.id.tmm.countstv.counting.PaperBundle.Origin.IneligibleCandidate
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, PreferenceTree}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.collection.immutable.{Bag, HashedBagConfiguration}

class PaperBundleSpec extends ImprovedFlatSpec {

  private implicit val bagConfiguration: HashedBagConfiguration[PaperBundle[Fruit]] =
    PaperBundle.bagConfiguration[Fruit]

  private val testPreferenceTree = PreferenceTree.from[Fruit](
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Banana, Pear),
  )

  "a paper bundle" should "have a transfer value" in {
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.transferValue === 0.5d)
  }

  it should "refer to a PreferenceTreeNode" in {
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.preferenceTreeNode eq testPreferenceTree.childFor(Fruit.Banana).get)
  }

  it should "have an origin" in {
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.origin === PaperBundle.Origin.InitialAllocation)
  }

  "a paper bundle's origin" can "be the initial allocation" in {
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = 0.5d,
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
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.assignedCandidate === Some(Fruit.Banana))
  }

  it can "not be assigned to a candidate" in {
    val paperBundle = ExhaustedPaperBundle[Fruit](
      numPapers = 10,
      transferValue = 0.5d,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.assignedCandidate === None)
  }

  private def testDistribution(
                                originalCandidate: Fruit,
                                appleStatus: CandidateStatus = CandidateStatus.Remaining,
                                bananaStatus: CandidateStatus = CandidateStatus.Remaining,
                                pearStatus: CandidateStatus = CandidateStatus.Remaining,
                                strawberryStatus: CandidateStatus = CandidateStatus.Remaining,
                                originalTransferValue: Double = 1d,
                                expectedBundlesAfterDistribution: Bag[PaperBundle[Fruit]] = Bag.empty[PaperBundle[Fruit]],
                                expectBundleUnchanged: Boolean = false,
                              ): Unit = {
    val candidateStatuses = CandidateStatuses[Fruit](
      Fruit.Apple -> appleStatus,
      Fruit.Banana -> bananaStatus,
      Fruit.Pear -> pearStatus,
      Fruit.Strawberry -> strawberryStatus,
    )

    val originalBundle = AssignedPaperBundle[Fruit](
      transferValue = originalTransferValue,
      preferenceTreeNode = testPreferenceTree.childFor(originalCandidate).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val origin = PaperBundle.Origin.IneligibleCandidate(originalCandidate)

    val actualBundlesAfterDistribution = originalBundle.distributeToRemainingCandidates(origin, candidateStatuses)

    if (expectBundleUnchanged) {
      assert(actualBundlesAfterDistribution === Bag[PaperBundle[Fruit]](originalBundle))
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
      expectedBundlesAfterDistribution = Bag(
        AssignedPaperBundle[Fruit](
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
      expectedBundlesAfterDistribution = Bag(
        AssignedPaperBundle(
          transferValue = 1d,
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
        AssignedPaperBundle(
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
      expectedBundlesAfterDistribution = Bag(
        AssignedPaperBundle(
          transferValue = 1d,
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
        AssignedPaperBundle(
          transferValue = 1d,
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Banana, Strawberry).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
      )
    )
  }

  it can "be distributed until exhausted" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Fruit.Apple -> CandidateStatus.Remaining,
      Fruit.Banana -> CandidateStatus.Excluded(ordinalExcluded = 0, excludedAtCount = 1),
      Fruit.Pear -> CandidateStatus.Excluded(ordinalExcluded = 1, excludedAtCount = 2),
      Fruit.Strawberry -> CandidateStatus.Remaining,
    )

    val originalBundle = AssignedPaperBundle[Fruit](
      transferValue = 1d,
      preferenceTreeNode = testPreferenceTree.childFor(Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val origin = PaperBundle.Origin.ExcludedCandidate(Pear)

    val actualBundlesAfterDistribution = originalBundle.distributeToRemainingCandidates(origin, candidateStatuses)

    val expectedBundlesAfterDistribution = Bag[PaperBundle[Fruit]](
      ExhaustedPaperBundle[Fruit](
        numPapers = 1,
        transferValue = 1d,
        origin = origin,
      ),
    )

    assert(actualBundlesAfterDistribution === expectedBundlesAfterDistribution)
  }

  it can "be distributed with a reduced transfer value" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Fruit.Apple -> CandidateStatus.Elected(ordinalElected = 0, electedAtCount = 1),
      Fruit.Banana -> CandidateStatus.Remaining,
      Fruit.Pear -> CandidateStatus.Remaining,
      Fruit.Strawberry -> CandidateStatus.Remaining,
    )

    val originalBundle = AssignedPaperBundle[Fruit](
      transferValue = 0.9,
      preferenceTreeNode = testPreferenceTree.childFor(Apple).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val origin = PaperBundle.Origin.ElectedCandidate(Apple, 0.7d)

    val actualBundlesAfterDistribution = originalBundle.distributeToRemainingCandidates(origin, candidateStatuses)

    val expectedBundlesAfterDistribution = Bag[PaperBundle[Fruit]](
      AssignedPaperBundle(
        transferValue = 0.9d * 0.7d,
        preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear).get,
        origin = origin,
      ),
      AssignedPaperBundle(
        transferValue = 0.9d * 0.7d,
        preferenceTreeNode = testPreferenceTree.childFor(Apple, Banana).get,
        origin = origin
      ),
    )

    assert(actualBundlesAfterDistribution === expectedBundlesAfterDistribution)
  }

  "a bundle of exhausted papers" can "not be distributed further" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Fruit.Apple -> CandidateStatus.Remaining,
      Fruit.Banana -> CandidateStatus.Ineligible,
      Fruit.Pear -> CandidateStatus.Ineligible,
      Fruit.Strawberry -> CandidateStatus.Remaining,
    )

    val originalBundle = ExhaustedPaperBundle[Fruit](
      numPapers = 1,
      transferValue = 1d,
      origin = IneligibleCandidate(Banana),
    )

    val origin: PaperBundle.Origin[Fruit] = PaperBundle.Origin.ElectedCandidate(Apple, 0.7d)

    val actualBundlesAfterDistribution = originalBundle.distributeToRemainingCandidates(origin, candidateStatuses)

    val expectedBundlesAfterDistribution = Bag[PaperBundle[Fruit]](originalBundle)

    assert(actualBundlesAfterDistribution === expectedBundlesAfterDistribution)
  }

  "a root paper bundle" can "be constructed from a PreferenceTree" in {
    val actualBundle = PaperBundle.rootBundleFor(testPreferenceTree)

    val expectedBundle = RootPaperBundle[Fruit](
      testPreferenceTree,
    )

    assert(actualBundle === expectedBundle)
  }

  it should "originate from the initial allocation" in {
    assert(PaperBundle.rootBundleFor(testPreferenceTree).origin === PaperBundle.Origin.InitialAllocation)
  }

  it should "not be assigned to a candidate" in {
    assert(PaperBundle.rootBundleFor(testPreferenceTree).assignedCandidate === None)
  }

  it should "return the number of ballots in the preference tree" in {
    assert(PaperBundle.rootBundleFor(testPreferenceTree).numPapers === testPreferenceTree.numPapers)
  }

  it should "have a transfer value of 1.0" in {
    assert(PaperBundle.rootBundleFor(testPreferenceTree).transferValue === 1.0d)
  }

  it can "distribute its papers" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Fruit.Apple -> CandidateStatus.Remaining,
      Fruit.Banana -> CandidateStatus.Remaining,
      Fruit.Pear -> CandidateStatus.Remaining,
      Fruit.Strawberry -> CandidateStatus.Remaining,
    )

    val actualBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, candidateStatuses)

    val expectedBundles = Bag[PaperBundle[Fruit]](
      AssignedPaperBundle(
        transferValue = 1d,
        preferenceTreeNode = testPreferenceTree.childFor(Apple).get,
        origin = PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle(
        transferValue = 1d,
        preferenceTreeNode = testPreferenceTree.childFor(Banana).get,
        origin = PaperBundle.Origin.InitialAllocation,
      )
    )

    assert(actualBundles === expectedBundles)
  }

  it can "distribute its papers regardless of their status" in {
    val actualBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distribute

    val expectedBundles = Bag[PaperBundle[Fruit]](
      AssignedPaperBundle(
        transferValue = 1d,
        preferenceTreeNode = testPreferenceTree.childFor(Apple).get,
        origin = PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle(
        transferValue = 1d,
        preferenceTreeNode = testPreferenceTree.childFor(Banana).get,
        origin = PaperBundle.Origin.InitialAllocation,
      )
    )

    assert(actualBundles === expectedBundles)
  }
}
