package au.id.tmm.countstv.counting.countsteps.distribution

import au.id.tmm.countstv.Fruit.{Mango, _}
import au.id.tmm.countstv.counting.countsteps.{CountContext, IneligibleHandling, InitialAllocationComputation}
import au.id.tmm.countstv.counting.{PaperBundle, RootPaperBundle}
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.{CountSteps, DistributionCountStep}
import au.id.tmm.countstv.model.values.Count
import au.id.tmm.countstv.model.{CandidateStatuses, PreferenceTree}
import au.id.tmm.countstv.{Fruit, NormalisedBallot}

trait DistributingPapersFixture {
  def ballots: Vector[NormalisedBallot[Fruit]]

  val testPreferenceTree: PreferenceTree[Fruit] = PreferenceTree.from[Fruit](ballots)

  val candidates: Set[Fruit] = Set(
    Apple,
    Banana,
    Pear,
    Strawberry,
    Mango,
    Raspberry,
    Watermelon,
  )

  val candidateStatuses: CandidateStatuses[Fruit] = CandidateStatuses[Fruit](
    candidates.map(_ -> Remaining).toMap
  )

  val rootBundle: RootPaperBundle[Fruit] = PaperBundle.rootBundleFor[Fruit](testPreferenceTree)

  val numVacancies: Int = 2

  val initialContext: CountContext[Fruit, CountSteps.Initial[Fruit]] =
    InitialAllocationComputation.computeInitialContext(
      candidateStatuses,
      rootBundle,
      numVacancies,
    )

  val contextAfterIneligibles: CountContext[Fruit, CountSteps.AfterIneligibleHandling[Fruit]] =
    IneligibleHandling.computeContextAfterIneligibles(
      initialContext,
    ).onlyOutcome

  def getActualCountStep(count: Count): DistributionCountStep[Fruit] = {
    firstContextAfterCount(count).previousCountSteps(count).asInstanceOf[DistributionCountStep[Fruit]]
  }

  def actualContextAfterCount(count: Count): CountContext[Fruit, CountSteps.DuringDistributions[Fruit]] = {
    val candidateContext = firstContextAfterCount(count)

    if (candidateContext.mostRecentCountStep.count == count) {
      candidateContext
    } else {
      throw new IllegalStateException(s"Context after $count is inaccessible")
    }
  }

  private def firstContextAfterCount(count: Count): CountContext[Fruit, CountSteps.DuringDistributions[Fruit]] = {
    require(count.asInt > 1)

    var newContext: CountContext[Fruit, CountSteps.DuringDistributions[Fruit]] =
      DistributingPapers.contextAfterNextCandidateDistribution(contextAfterIneligibles).onlyOutcome

    while (newContext.mostRecentCountStep.count.asInt < count.asInt) {
      newContext = DistributingPapers.contextAfterNextCandidateDistribution(newContext).onlyOutcome
    }

    newContext
  }

}


object DistributingPapersFixture {

  object WithFinalElection extends DistributingPapersFixture {
    override def ballots = Vector(
      Vector(Apple, Banana, Strawberry, Pear, Raspberry, Mango, Watermelon),
      Vector(Apple, Pear, Mango, Strawberry, Banana, Raspberry, Watermelon),
      Vector(Apple, Pear, Mango, Strawberry, Raspberry, Watermelon, Banana),
      Vector(Apple, Mango, Watermelon, Pear, Banana, Strawberry, Raspberry),
      Vector(Apple, Raspberry, Mango, Strawberry, Pear, Banana, Watermelon),
      Vector(Apple, Raspberry, Mango, Pear, Strawberry, Banana, Watermelon),
      Vector(Apple, Strawberry, Pear, Raspberry, Watermelon, Mango, Banana),
      Vector(Apple, Strawberry, Watermelon, Raspberry, Pear, Mango, Banana),
      Vector(Apple, Strawberry, Mango, Raspberry, Watermelon, Pear, Banana),
      Vector(Apple, Watermelon, Banana, Pear, Strawberry, Mango, Raspberry), // 10 votes for Apple
      Vector(Banana, Apple, Strawberry, Mango, Raspberry, Watermelon, Pear),
      Vector(Banana, Apple, Raspberry, Mango, Watermelon, Pear, Strawberry),
      Vector(Banana, Apple, Watermelon, Raspberry, Pear, Strawberry, Mango),
      Vector(Banana, Raspberry, Pear, Mango, Strawberry, Apple, Watermelon),
      Vector(Banana, Strawberry, Apple, Watermelon, Raspberry, Pear, Mango),
      Vector(Banana, Watermelon, Apple, Mango, Raspberry, Strawberry, Pear), // 6 votes for Banana
      Vector(Pear, Apple, Banana, Mango, Watermelon, Raspberry, Strawberry),
      Vector(Pear, Banana, Apple, Mango, Strawberry, Watermelon, Raspberry),
      Vector(Pear, Raspberry, Banana, Strawberry, Watermelon, Apple, Mango),
      Vector(Pear, Raspberry, Strawberry, Mango, Watermelon, Apple, Banana),
      Vector(Pear, Strawberry, Raspberry, Banana, Apple, Mango, Watermelon),
      Vector(Pear, Strawberry, Apple, Mango, Watermelon, Banana, Raspberry),
      Vector(Pear, Strawberry, Raspberry, Banana, Mango, Apple, Watermelon),
      Vector(Pear, Watermelon, Banana, Mango, Strawberry, Raspberry, Apple),
      Vector(Pear, Watermelon, Mango, Apple, Strawberry, Banana, Raspberry), // 9 votes for Pear
      Vector(Mango, Apple, Pear, Strawberry, Banana, Raspberry, Watermelon),
      Vector(Mango, Apple, Watermelon, Raspberry, Pear, Strawberry, Banana),
      Vector(Mango, Apple, Pear, Raspberry, Watermelon, Banana, Strawberry),
      Vector(Mango, Apple, Watermelon, Raspberry, Strawberry, Pear, Banana),
      Vector(Mango, Banana, Apple, Pear, Watermelon, Strawberry, Raspberry),
      Vector(Mango, Banana, Strawberry, Pear, Apple, Raspberry, Watermelon),
      Vector(Mango, Pear, Raspberry, Strawberry, Watermelon, Banana, Apple),
      Vector(Mango, Watermelon, Pear, Raspberry, Apple, Banana, Strawberry),
      Vector(Mango, Watermelon, Pear, Banana, Strawberry, Raspberry, Apple), // 9 votes for Mango
      Vector(Raspberry, Banana, Watermelon, Strawberry, Pear, Apple, Mango),
      Vector(Raspberry, Mango, Pear, Watermelon, Banana, Apple, Strawberry),
      Vector(Raspberry, Mango, Apple, Pear, Banana, Watermelon, Strawberry),
      Vector(Raspberry, Mango, Banana, Apple, Watermelon, Strawberry, Pear),
      Vector(Raspberry, Strawberry, Apple, Pear, Watermelon, Mango, Banana),
      Vector(Raspberry, Watermelon, Strawberry, Apple, Banana, Pear, Mango),
      Vector(Raspberry, Watermelon, Pear, Apple, Banana, Strawberry, Mango), // 7 votes for Raspberry
      Vector(Strawberry, Apple, Pear, Watermelon, Mango, Raspberry, Banana),
      Vector(Strawberry, Banana, Apple, Mango, Watermelon, Raspberry, Pear),
      Vector(Strawberry, Banana, Mango, Pear, Apple, Watermelon, Raspberry),
      Vector(Strawberry, Raspberry, Apple, Banana, Mango, Pear, Watermelon),
      Vector(Strawberry, Watermelon, Raspberry, Mango, Apple, Pear, Banana), // 5 votes for Strawberry
      Vector(Watermelon, Apple, Raspberry, Mango, Banana, Pear, Strawberry),
      Vector(Watermelon, Pear, Apple, Banana, Raspberry, Mango, Strawberry),
      Vector(Watermelon, Pear, Apple, Mango, Raspberry, Banana, Strawberry),
      Vector(Watermelon, Mango, Pear, Raspberry, Apple, Banana, Strawberry), // 4 votes for Watermelon
    )
  }

  object WithVotelessCandidate extends DistributingPapersFixture {
    override def ballots: Vector[Vector[Fruit]] = WithFinalElection.ballots.map { ballot =>
      if (ballot.head == Watermelon) {
        ballot.tail
      } else {
        ballot
      }
    }
  }

}
