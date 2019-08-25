package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit.{Apple, Banana, Pear, Strawberry}
import au.id.tmm.countstv.counting.PaperBundle.Origin.IneligibleCandidate
import au.id.tmm.countstv.model.preferences.PreferenceTree
import au.id.tmm.countstv.model.values._
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses}
import org.scalatest.FlatSpec
import org.scalatest.Assertion

class PaperBundleSpec extends FlatSpec {

  private val testPreferenceTree = PreferenceTree.from[Fruit](Set(Apple, Pear, Banana, Strawberry), numBallotsHint = 3)(List(
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Banana, Pear),
  ))

  "a paper bundle" should "have a transfer value" in {
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = TransferValue(0.5d),
      preferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.transferValue === TransferValue(0.5d))
  }

  it should "refer to a PreferenceTreeNode" in {
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = TransferValue(0.5d),
      preferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.preferenceTreeNode === testPreferenceTree.childFor(Fruit.Banana).get)
  }

  it should "have an origin" in {
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = TransferValue(0.5d),
      preferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.origin === PaperBundle.Origin.InitialAllocation)
  }

  "a paper bundle" should "be assigned to a candidate" in {
    val paperBundle = AssignedPaperBundle[Fruit](
      transferValue = TransferValue(0.5d),
      preferenceTreeNode = testPreferenceTree.childFor(Fruit.Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    assert(paperBundle.assignedCandidate === Some(Fruit.Banana))
  }

  it can "not be assigned to a candidate" in {
    val paperBundle = ExhaustedPaperBundle[Fruit](
      numPapers = NumPapers(10),
      transferValue = TransferValue(0.5d),
      origin = PaperBundle.Origin.InitialAllocation,
      exhaustedAtCount = Count(1),
      originatingNode = testPreferenceTree.childFor(Fruit.Banana).get,
    )

    assert(paperBundle.assignedCandidate === None)
  }

  private def testDistribution(
                                originalCandidate: Fruit,
                                appleStatus: CandidateStatus = CandidateStatus.Remaining,
                                bananaStatus: CandidateStatus = CandidateStatus.Remaining,
                                pearStatus: CandidateStatus = CandidateStatus.Remaining,
                                strawberryStatus: CandidateStatus = CandidateStatus.Remaining,
                                originalTransferValue: TransferValue = TransferValue(1d),
                                expectedBundlesAfterDistribution: Set[PaperBundle[Fruit]] = Set.empty[PaperBundle[Fruit]],
                                expectBundleUnchanged: Boolean = false,
                              ): Assertion = {
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

    val actualBundlesAfterDistribution = originalBundle.distributeToRemainingCandidates(origin, Count(1), candidateStatuses)

    if (expectBundleUnchanged) {
      assert(actualBundlesAfterDistribution === Set[PaperBundle[Fruit]](originalBundle))
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
        AssignedPaperBundle[Fruit](
          transferValue = TransferValue(1d),
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
        AssignedPaperBundle(
          transferValue = TransferValue(1d),
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
        AssignedPaperBundle(
          transferValue = TransferValue(1d),
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
        AssignedPaperBundle(
          transferValue = TransferValue(1d),
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
        AssignedPaperBundle(
          transferValue = TransferValue(1d),
          preferenceTreeNode = testPreferenceTree.childFor(Apple, Banana, Strawberry).get,
          origin = PaperBundle.Origin.IneligibleCandidate(Apple),
        ),
      )
    )
  }

  it can "be distributed until exhausted" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Fruit.Apple -> CandidateStatus.Remaining,
      Fruit.Banana -> CandidateStatus.Excluded(Ordinal.first, excludedAtCount = Count(1)),
      Fruit.Pear -> CandidateStatus.Excluded(Ordinal.second, excludedAtCount = Count(2)),
      Fruit.Strawberry -> CandidateStatus.Remaining,
    )

    val originalBundle = AssignedPaperBundle[Fruit](
      transferValue = TransferValue(1d),
      preferenceTreeNode = testPreferenceTree.childFor(Banana).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val origin = PaperBundle.Origin.ExcludedCandidate(Pear, Count(2))

    val actualBundlesAfterDistribution = originalBundle.distributeToRemainingCandidates(origin, Count(2), candidateStatuses)

    val expectedBundlesAfterDistribution = Set[PaperBundle[Fruit]](
      ExhaustedPaperBundle[Fruit](
        numPapers = NumPapers(1),
        transferValue = TransferValue(1d),
        origin = origin,
        exhaustedAtCount = Count(2),
        originatingNode = originalBundle.preferenceTreeNode,
      ),
    )

    assert(actualBundlesAfterDistribution === expectedBundlesAfterDistribution)
  }

  it can "be distributed with a reduced transfer value" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Fruit.Apple -> CandidateStatus.Elected(Ordinal.first, electedAtCount = Count(1)),
      Fruit.Banana -> CandidateStatus.Remaining,
      Fruit.Pear -> CandidateStatus.Remaining,
      Fruit.Strawberry -> CandidateStatus.Remaining,
    )

    val originalBundle = AssignedPaperBundle[Fruit](
      transferValue = TransferValue(0.9),
      preferenceTreeNode = testPreferenceTree.childFor(Apple).get,
      origin = PaperBundle.Origin.InitialAllocation,
    )

    val transferValue = TransferValue(0.7d)
    val origin = PaperBundle.Origin.ElectedCandidate(Apple, transferValue, Count(1))

    val actualBundlesAfterDistribution = originalBundle.distributeToRemainingCandidates(origin, Count(1), candidateStatuses)

    val expectedBundlesAfterDistribution = Set[PaperBundle[Fruit]](
      AssignedPaperBundle(
        transferValue = transferValue,
        preferenceTreeNode = testPreferenceTree.childFor(Apple, Pear).get,
        origin = origin,
      ),
      AssignedPaperBundle(
        transferValue = transferValue,
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
      numPapers = NumPapers(1),
      transferValue = TransferValue(1d),
      origin = IneligibleCandidate(Banana),
      exhaustedAtCount = Count(1),
      originatingNode = testPreferenceTree.childFor(Fruit.Banana).get,
    )

    val origin: PaperBundle.Origin[Fruit] =
      PaperBundle.Origin.ElectedCandidate(Apple, TransferValue(0.7d), Count(1))

    val actualBundlesAfterDistribution = originalBundle.distributeToRemainingCandidates(origin, Count(1), candidateStatuses)

    val expectedBundlesAfterDistribution = Set[PaperBundle[Fruit]](originalBundle)

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
    assert(PaperBundle.rootBundleFor(testPreferenceTree).transferValue === TransferValue(1.0d))
  }

  it can "distribute its papers" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Fruit.Apple -> CandidateStatus.Remaining,
      Fruit.Banana -> CandidateStatus.Remaining,
      Fruit.Pear -> CandidateStatus.Remaining,
      Fruit.Strawberry -> CandidateStatus.Remaining,
    )

    val actualBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, Count(1), candidateStatuses)

    val expectedBundles = Set[PaperBundle[Fruit]](
      AssignedPaperBundle(
        transferValue = TransferValue(1d),
        preferenceTreeNode = testPreferenceTree.childFor(Apple).get,
        origin = PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle(
        transferValue = TransferValue(1d),
        preferenceTreeNode = testPreferenceTree.childFor(Banana).get,
        origin = PaperBundle.Origin.InitialAllocation,
      )
    )

    assert(actualBundles === expectedBundles)
  }

  it can "distribute its papers regardless of their status" in {
    val actualBundles = PaperBundle.rootBundleFor(testPreferenceTree)
      .distribute

    val expectedBundles = Set[PaperBundle[Fruit]](
      AssignedPaperBundle(
        transferValue = TransferValue(1d),
        preferenceTreeNode = testPreferenceTree.childFor(Apple).get,
        origin = PaperBundle.Origin.InitialAllocation,
      ),
      AssignedPaperBundle(
        transferValue = TransferValue(1d),
        preferenceTreeNode = testPreferenceTree.childFor(Banana).get,
        origin = PaperBundle.Origin.InitialAllocation,
      )
    )

    assert(actualBundles === expectedBundles)
  }
}
