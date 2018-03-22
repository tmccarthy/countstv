package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.InitialAllocation
import au.id.tmm.countstv.model.values.{Count, NumPapers, Ordinal}
import au.id.tmm.countstv.model.{PaperBundle, _}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class InitialAllocationComputationSpec extends ImprovedFlatSpec {

  private val testPreferenceTree = PreferenceTree.from[Fruit](
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
  )

  private val candidateStatuses = CandidateStatuses[Fruit](
    Apple -> Remaining,
    Banana -> Remaining,
    Pear -> Remaining,
    Strawberry -> Remaining,
  )

  private val rootBundle = PaperBundle.rootBundleFor[Fruit](testPreferenceTree)

  private val numVacancies: Int = 2

  "an initial allocation" can "not be computed if a candidate is elected" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Ineligible,
      Banana -> Elected(Ordinal.first, electedAtCount = Count(1)),
      Pear -> Remaining,
      Strawberry -> Remaining,
    )

    intercept[IllegalArgumentException] {
      InitialAllocationComputation.computeInitialContext(
        candidateStatuses,
        rootBundle,
        numVacancies,
      )
    }
  }

  it can "not be computed if a candidate is excluded" in {
    val candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Ineligible,
      Banana -> Excluded(Ordinal.first, excludedAtCount = Count(1)),
      Pear -> Remaining,
      Strawberry -> Remaining,
    )

    intercept[IllegalArgumentException] {
      InitialAllocationComputation.computeInitialContext(
        candidateStatuses,
        rootBundle,
        numVacancies,
      )
    }
  }

  it should "produce the correct context" in {
    val actualContext = InitialAllocationComputation.computeInitialContext(
      candidateStatuses,
      rootBundle,
      numVacancies,
    )

    val expectedContext = CountContext[Fruit](
      numFormalPapers = NumPapers(25),
      numVacancies = 2,
      paperBundles = rootBundle.distribute,
      previousCountSteps = List(
        InitialAllocation(
          candidateStatuses = candidateStatuses,
          candidateVoteCounts = CandidateVoteCounts[Fruit](
            perCandidate = Map(
              Apple -> VoteCount(6),
              Banana -> VoteCount(6),
              Pear -> VoteCount(8),
              Strawberry -> VoteCount(5),
            ),
            exhausted = VoteCount.zero,
            roundingError = VoteCount.zero
          ),
        )
      ),
      currentDistribution = None,
    )

    assert(actualContext === expectedContext)
  }

}
