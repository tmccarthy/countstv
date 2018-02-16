package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.values.{Count, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatuses, PreferenceTree}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class FullCountComputationSpec extends ImprovedFlatSpec {

  private val testPreferenceTree = PreferenceTree.from[Fruit](
    Vector(Apple, Banana, Strawberry, Pear, Raspberry, Mango, Watermelon),
    Vector(Apple, Pear, Mango, Strawberry, Banana, Raspberry, Watermelon),
    Vector(Apple, Pear, Mango, Strawberry, Raspberry, Watermelon, Banana),
    Vector(Apple, Mango, Watermelon, Pear, Banana, Strawberry, Raspberry),
    Vector(Apple, Raspberry, Mango, Strawberry, Pear, Banana, Watermelon),
    Vector(Apple, Raspberry, Mango, Pear, Strawberry, Banana, Watermelon),
    Vector(Apple, Strawberry, Pear, Raspberry, Watermelon, Mango, Banana),
    Vector(Apple, Strawberry, Watermelon, Raspberry, Pear, Mango, Banana),
    Vector(Apple, Strawberry, Mango, Raspberry, Watermelon, Pear, Banana),
    Vector(Apple, Watermelon, Banana, Pear, Strawberry, Mango, Raspberry),
    Vector(Banana, Apple, Strawberry, Mango, Raspberry, Watermelon, Pear),
    Vector(Banana, Apple, Raspberry, Mango, Watermelon, Pear, Strawberry),
    Vector(Banana, Apple, Watermelon, Raspberry, Pear, Strawberry, Mango),
    Vector(Banana, Raspberry, Pear, Mango, Strawberry, Apple, Watermelon),
    Vector(Banana, Strawberry, Apple, Watermelon, Raspberry, Pear, Mango),
    Vector(Banana, Watermelon, Apple, Mango, Raspberry, Strawberry, Pear),
    Vector(Pear, Apple, Banana, Mango, Watermelon, Raspberry, Strawberry),
    Vector(Pear, Banana, Apple, Mango, Strawberry, Watermelon, Raspberry),
    Vector(Pear, Raspberry, Banana, Strawberry, Watermelon, Apple, Mango),
    Vector(Pear, Raspberry, Strawberry, Mango, Watermelon, Apple, Banana),
    Vector(Pear, Strawberry, Raspberry, Banana, Apple, Mango, Watermelon),
    Vector(Pear, Strawberry, Apple, Mango, Watermelon, Banana, Raspberry),
    Vector(Pear, Strawberry, Raspberry, Banana, Mango, Apple, Watermelon),
    Vector(Pear, Watermelon, Banana, Mango, Strawberry, Raspberry, Apple),
    Vector(Pear, Watermelon, Mango, Apple, Strawberry, Banana, Raspberry),
    Vector(Mango, Apple, Pear, Strawberry, Banana, Raspberry, Watermelon),
    Vector(Mango, Apple, Watermelon, Raspberry, Pear, Strawberry, Banana),
    Vector(Mango, Apple, Pear, Raspberry, Watermelon, Banana, Strawberry),
    Vector(Mango, Apple, Watermelon, Raspberry, Strawberry, Pear, Banana),
    Vector(Mango, Banana, Apple, Pear, Watermelon, Strawberry, Raspberry),
    Vector(Mango, Banana, Strawberry, Pear, Apple, Raspberry, Watermelon),
    Vector(Mango, Pear, Raspberry, Strawberry, Watermelon, Banana, Apple),
    Vector(Mango, Watermelon, Pear, Raspberry, Apple, Banana, Strawberry),
    Vector(Mango, Watermelon, Pear, Banana, Strawberry, Raspberry, Apple),
    Vector(Raspberry, Banana, Watermelon, Strawberry, Pear, Apple, Mango),
    Vector(Raspberry, Mango, Pear, Watermelon, Banana, Apple, Strawberry),
    Vector(Raspberry, Mango, Apple, Pear, Banana, Watermelon, Strawberry),
    Vector(Raspberry, Mango, Banana, Apple, Watermelon, Strawberry, Pear),
    Vector(Raspberry, Strawberry, Apple, Pear, Watermelon, Mango, Banana),
    Vector(Raspberry, Watermelon, Strawberry, Apple, Banana, Pear, Mango),
    Vector(Raspberry, Watermelon, Pear, Apple, Banana, Strawberry, Mango),
    Vector(Strawberry, Apple, Pear, Watermelon, Mango, Raspberry, Banana),
    Vector(Strawberry, Banana, Apple, Mango, Watermelon, Raspberry, Pear),
    Vector(Strawberry, Banana, Mango, Pear, Apple, Watermelon, Raspberry),
    Vector(Strawberry, Raspberry, Apple, Banana, Mango, Pear, Watermelon),
    Vector(Strawberry, Watermelon, Raspberry, Mango, Apple, Pear, Banana),
    Vector(Watermelon, Apple, Raspberry, Mango, Banana, Pear, Strawberry),
    Vector(Watermelon, Pear, Apple, Banana, Raspberry, Mango, Strawberry),
    Vector(Watermelon, Pear, Apple, Mango, Raspberry, Banana, Strawberry),
    Vector(Watermelon, Mango, Pear, Raspberry, Apple, Banana, Strawberry),
  )

  private val candidates: Set[Fruit] = Set(
    Apple,
    Banana,
    Pear,
    Strawberry,
    Mango,
    Raspberry,
    Watermelon,
  )

  private val numVacancies = 2

  "a full vote count" should "produce the correct outcome" in {
    val countSteps = FullCountComputation.runCount[Fruit](
      candidates,
      ineligibleCandidates = Set.empty[Fruit],
      numVacancies,
      testPreferenceTree,
    ).anyOutcome

    val expectedFinalOutcomes = CandidateStatuses[Fruit](
      Apple -> Elected(Ordinal.first,Count(4)),
      Banana -> Excluded(Ordinal.third,Count(4)),
      Mango -> Remaining,
      Pear -> Elected(Ordinal.second,Count(8)),
      Raspberry -> Excluded(Ordinal.fourth,Count(6)),
      Strawberry -> Excluded(Ordinal.second,Count(3)),
      Watermelon -> Excluded(Ordinal.first,Count(2)),
    )

    assert(countSteps.last.candidateStatuses === expectedFinalOutcomes)
  }

  it should "produce the correct outcome when we have ineligible candidates" in {
    val countSteps = FullCountComputation.runCount[Fruit](
      candidates,
      ineligibleCandidates = Set[Fruit](Apple),
      numVacancies,
      testPreferenceTree,
    ).anyOutcome

    val expectedFinalOutcomes = CandidateStatuses[Fruit](
      Apple -> Ineligible,
      Banana -> Excluded(Ordinal.second, Count(3)),
      Mango -> Remaining,
      Pear -> Elected(Ordinal.first, Count(4)),
      Raspberry -> Elected(Ordinal.second, Count(4)),
      Strawberry -> Excluded(Ordinal.third, Count(4)),
      Watermelon -> Excluded(Ordinal.first, Count(2)),
    )

    assert(countSteps.last.candidateStatuses === expectedFinalOutcomes)
  }

  it can "not occur if the set of ineligible candidates is not a subset of the set of all candidates" in {
    intercept[IllegalArgumentException] {
      FullCountComputation.runCount[Fruit](
        candidates,
        ineligibleCandidates = Set[Fruit](Peach),
        numVacancies = 2,
        testPreferenceTree,
      )
    }
  }
}
