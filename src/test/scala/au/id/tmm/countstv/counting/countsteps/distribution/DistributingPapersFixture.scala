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
    override def ballots: Vector[Vector[Fruit]] = Vector[Vector[Fruit]](
      Vector[Fruit](Apple, Banana, Strawberry, Pear, Raspberry, Mango, Watermelon),
      Vector[Fruit](Apple, Pear, Mango, Strawberry, Banana, Raspberry, Watermelon),
      Vector[Fruit](Apple, Pear, Mango, Strawberry, Raspberry, Watermelon, Banana),
      Vector[Fruit](Apple, Mango, Watermelon, Pear, Banana, Strawberry, Raspberry),
      Vector[Fruit](Apple, Raspberry, Mango, Strawberry, Pear, Banana, Watermelon),
      Vector[Fruit](Apple, Raspberry, Mango, Pear, Strawberry, Banana, Watermelon),
      Vector[Fruit](Apple, Strawberry, Pear, Raspberry, Watermelon, Mango, Banana),
      Vector[Fruit](Apple, Strawberry, Watermelon, Raspberry, Pear, Mango, Banana),
      Vector[Fruit](Apple, Strawberry, Mango, Raspberry, Watermelon, Pear, Banana),
      Vector[Fruit](Apple, Watermelon, Banana, Pear, Strawberry, Mango, Raspberry), // 10 votes for Apple
      Vector[Fruit](Banana, Apple, Strawberry, Mango, Raspberry, Watermelon, Pear),
      Vector[Fruit](Banana, Apple, Raspberry, Mango, Watermelon, Pear, Strawberry),
      Vector[Fruit](Banana, Apple, Watermelon, Raspberry, Pear, Strawberry, Mango),
      Vector[Fruit](Banana, Raspberry, Pear, Mango, Strawberry, Apple, Watermelon),
      Vector[Fruit](Banana, Strawberry, Apple, Watermelon, Raspberry, Pear, Mango),
      Vector[Fruit](Banana, Watermelon, Apple, Mango, Raspberry, Strawberry, Pear), // 6 votes for Banana
      Vector[Fruit](Pear, Apple, Banana, Mango, Watermelon, Raspberry, Strawberry),
      Vector[Fruit](Pear, Banana, Apple, Mango, Strawberry, Watermelon, Raspberry),
      Vector[Fruit](Pear, Raspberry, Banana, Strawberry, Watermelon, Apple, Mango),
      Vector[Fruit](Pear, Raspberry, Strawberry, Mango, Watermelon, Apple, Banana),
      Vector[Fruit](Pear, Strawberry, Raspberry, Banana, Apple, Mango, Watermelon),
      Vector[Fruit](Pear, Strawberry, Apple, Mango, Watermelon, Banana, Raspberry),
      Vector[Fruit](Pear, Strawberry, Raspberry, Banana, Mango, Apple, Watermelon),
      Vector[Fruit](Pear, Watermelon, Banana, Mango, Strawberry, Raspberry, Apple),
      Vector[Fruit](Pear, Watermelon, Mango, Apple, Strawberry, Banana, Raspberry), // 9 votes for Pear
      Vector[Fruit](Mango, Apple, Pear, Strawberry, Banana, Raspberry, Watermelon),
      Vector[Fruit](Mango, Apple, Watermelon, Raspberry, Pear, Strawberry, Banana),
      Vector[Fruit](Mango, Apple, Pear, Raspberry, Watermelon, Banana, Strawberry),
      Vector[Fruit](Mango, Apple, Watermelon, Raspberry, Strawberry, Pear, Banana),
      Vector[Fruit](Mango, Banana, Apple, Pear, Watermelon, Strawberry, Raspberry),
      Vector[Fruit](Mango, Banana, Strawberry, Pear, Apple, Raspberry, Watermelon),
      Vector[Fruit](Mango, Pear, Raspberry, Strawberry, Watermelon, Banana, Apple),
      Vector[Fruit](Mango, Watermelon, Pear, Raspberry, Apple, Banana, Strawberry),
      Vector[Fruit](Mango, Watermelon, Pear, Banana, Strawberry, Raspberry, Apple), // 9 votes for Mango
      Vector[Fruit](Raspberry, Banana, Watermelon, Strawberry, Pear, Apple, Mango),
      Vector[Fruit](Raspberry, Mango, Pear, Watermelon, Banana, Apple, Strawberry),
      Vector[Fruit](Raspberry, Mango, Apple, Pear, Banana, Watermelon, Strawberry),
      Vector[Fruit](Raspberry, Mango, Banana, Apple, Watermelon, Strawberry, Pear),
      Vector[Fruit](Raspberry, Strawberry, Apple, Pear, Watermelon, Mango, Banana),
      Vector[Fruit](Raspberry, Watermelon, Strawberry, Apple, Banana, Pear, Mango),
      Vector[Fruit](Raspberry, Watermelon, Pear, Apple, Banana, Strawberry, Mango), // 7 votes for Raspberry
      Vector[Fruit](Strawberry, Apple, Pear, Watermelon, Mango, Raspberry, Banana),
      Vector[Fruit](Strawberry, Banana, Apple, Mango, Watermelon, Raspberry, Pear),
      Vector[Fruit](Strawberry, Banana, Mango, Pear, Apple, Watermelon, Raspberry),
      Vector[Fruit](Strawberry, Raspberry, Apple, Banana, Mango, Pear, Watermelon),
      Vector[Fruit](Strawberry, Watermelon, Raspberry, Mango, Apple, Pear, Banana), // 5 votes for Strawberry
      Vector[Fruit](Watermelon, Apple, Raspberry, Mango, Banana, Pear, Strawberry),
      Vector[Fruit](Watermelon, Pear, Apple, Banana, Raspberry, Mango, Strawberry),
      Vector[Fruit](Watermelon, Pear, Apple, Mango, Raspberry, Banana, Strawberry),
      Vector[Fruit](Watermelon, Mango, Pear, Raspberry, Apple, Banana, Strawberry), // 4 votes for Watermelon
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

  object WithElectionSansSurplus extends DistributingPapersFixture {
    override def ballots: Vector[NormalisedBallot[Fruit]] = WithFinalElection.ballots.map { ballot =>
      if (ballot.take(2) == Vector(Watermelon, Apple)) {
        ballot.head +: ballot.drop(2)
      } else {
        ballot
      }
    }
  }

}
