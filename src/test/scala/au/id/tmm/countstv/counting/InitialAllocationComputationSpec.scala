package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, PreferenceTree, VoteCount}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class InitialAllocationComputationSpec extends ImprovedFlatSpec {

  private val testPreferenceTree = PreferenceTree.from(
    Vector(Apple, Banana, Pear, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Apple, Pear, Banana),
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Strawberry, Pear),
    Vector(Banana),
    Vector(Banana, Apple),
    Vector(Banana, Apple, Strawberry, Pear),
    Vector(Banana, Strawberry, Apple, Pear),
    Vector(Banana, Strawberry, Pear, Apple),
    Vector(Banana, Strawberry, Pear, Apple),
    Vector(Pear, Apple),
    Vector(Pear, Apple, Banana, Strawberry),
    Vector(Pear, Apple, Banana, Strawberry),
    Vector(Pear, Banana, Apple, Strawberry),
    Vector(Pear, Banana, Strawberry, Apple),
    Vector(Pear, Banana, Strawberry, Apple),
    Vector(Pear, Strawberry, Banana, Apple),
    Vector(Pear, Strawberry, Banana, Apple),
    Vector(Strawberry),
    Vector(Strawberry, Apple, Pear),
    Vector(Strawberry, Apple, Pear, Banana),
    Vector(Strawberry, Banana, Apple, Pear),
    Vector(Strawberry, Pear, Apple, Banana),
    Vector(),
    Vector(),
  )

  private val candidateStatuses = CandidateStatuses(
    Apple -> Remaining,
    Banana -> Remaining,
    Pear -> Remaining,
    Strawberry -> Remaining,
  )

  private val paperBundles = PaperBundle.rootBundleFor(testPreferenceTree)
    .distributeToRemainingCandidates(PaperBundle.Origin.InitialAllocation, candidateStatuses)

  private val numPapers: Long = testPreferenceTree.numPapers
  private val quota = QuotaComputation.computeQuota(numVacancies = 2, numFormalPapers = numPapers)

  "an initial allocation" can "not be computed if a candidate is elected" in {
    val candidateStatuses = CandidateStatuses(
      Apple -> Ineligible,
      Banana -> Elected(ordinalElected = 0, electedAtCount = 1),
      Pear -> Remaining,
      Strawberry -> Remaining,
    )

    intercept[IllegalArgumentException] {
      InitialAllocationComputation.computeInitialAllocation(
        candidateStatuses,
        quota,
        numPapers,
        paperBundles,
      )
    }
  }

  it can "not be computed if a candidate is excluded" in {
    val candidateStatuses = CandidateStatuses(
      Apple -> Ineligible,
      Banana -> Excluded(ordinalExcluded = 0, excludedAtCount = 1),
      Pear -> Remaining,
      Strawberry -> Remaining,
    )

    intercept[IllegalArgumentException] {
      InitialAllocationComputation.computeInitialAllocation(
        candidateStatuses,
        quota,
        numPapers,
        paperBundles,
      )
    }
  }

  it should "have the correct count correctly calculated" in {
    val initialAllocation = InitialAllocationComputation.computeInitialAllocation(
      candidateStatuses,
      quota,
      numPapers,
      paperBundles,
    )

    val actualCount = initialAllocation.candidateVoteCounts

    val expectedCount = CandidateVoteCounts(
      perCandidate = Map(
        Apple -> VoteCount(6, 6),
        Banana -> VoteCount(6, 6),
        Pear -> VoteCount(8, 8),
        Strawberry -> VoteCount(5, 5),
      ),
      exhausted = VoteCount(2, 2),
      roundingError = VoteCount.zero
    )

    assert(actualCount === expectedCount)
  }

}
