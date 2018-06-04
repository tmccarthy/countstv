package au.id.tmm.countstv.counting.fixtures

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.countsteps.distribution.DistributingPapers
import au.id.tmm.countstv.counting.countsteps.{CountContext, IneligibleHandling, InitialAllocationComputation}
import au.id.tmm.countstv.counting.{PaperBundle, RootPaperBundle}
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.{CountSteps, DistributionPhaseCountStep}
import au.id.tmm.countstv.model.values.{Count, NumPapers}
import au.id.tmm.countstv.model.{CandidateStatuses, PreferenceTree}

case class CountFixture(
                         allBallots: Vector[Vector[Fruit]],
                         candidates: Set[Fruit] = Set(
                           Apple,
                           Banana,
                           Pear,
                           Strawberry,
                           Mango,
                           Raspberry,
                           Watermelon,
                         ),
                         numVacancies: Int = 2,
                         ineligibleCandidates: Set[Fruit] = Set.empty
                       ) {

  val numPapers: NumPapers = NumPapers(allBallots.size)

  lazy val preferenceTree: PreferenceTree[Fruit] = PreferenceTree.from(allBallots)

  lazy val initialContext: CountContext[Fruit, CountSteps.Initial[Fruit]] = {
    val rootBundle: RootPaperBundle[Fruit] = PaperBundle.rootBundleFor[Fruit](preferenceTree)

    val initialCandidateStatuses = CandidateStatuses(
      candidates.map { candidate =>
        if (ineligibleCandidates contains candidate) {
          candidate -> Ineligible
        } else {
          candidate -> Remaining
        }
      }.toMap
    )

    InitialAllocationComputation.computeInitialContext(initialCandidateStatuses, rootBundle, numVacancies)
  }

  lazy val contextAfterIneligibles: CountContext[Fruit, CountSteps.AfterIneligibleHandling[Fruit]] =
    IneligibleHandling.computeContextAfterIneligibles(
      initialContext,
    ).onlyOutcome

  def getActualCountStep(count: Count): DistributionPhaseCountStep[Fruit] = {
    firstContextAfterCount(count).previousCountSteps(count).asInstanceOf[DistributionPhaseCountStep[Fruit]]
  }

  // TODO this should be generalised for all countsteps types, and provide a specific method for distributions
  // (with an assertion)
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

  def afterEditingBallots(editBallots: Vector[Vector[Fruit]] => Vector[Vector[Fruit]]): CountFixture = {
    val editedBallots = editBallots(this.allBallots)

    CountFixture(editedBallots)
  }

}

object CountFixture {

  def apply(allBallots: Vector[Fruit]*): CountFixture = new CountFixture(allBallots.toVector)

  val withFinalElection = CountFixture(
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

  val withVotelessCandidate: CountFixture = withFinalElection.afterEditingBallots { ballots =>
    // Delete the first preference of any ballot that preferences Watermelon first
    ballots.map { ballot =>
      if (ballot.head == Watermelon) {
        ballot.tail
      } else {
        ballot
      }
    }
  }

  val withElectionSansSurplus: CountFixture = withFinalElection.afterEditingBallots { ballots =>
    //** Find the ballot that preferences Watermelon and then Apple, and drop the preference for Apple.
    ballots.map { ballot =>
      if (ballot.take(2) == Vector(Watermelon, Apple)) {
        ballot.head +: ballot.drop(2)
      } else {
        ballot
      }
    }
  }

  val withOneRemainingCandidate = CountFixture(
    Vector(Apple, Banana, Strawberry, Pear, Raspberry, Mango, Watermelon),
    Vector(Apple, Pear, Mango, Strawberry, Banana, Raspberry, Watermelon),
    Vector(Apple, Pear, Mango, Strawberry, Raspberry, Watermelon, Banana),
    Vector(Apple, Mango, Watermelon, Pear, Banana, Strawberry, Raspberry),
    Vector(Apple, Raspberry, Mango, Strawberry, Pear, Banana, Watermelon),
    Vector(Apple, Raspberry, Mango, Pear, Strawberry, Banana, Watermelon),
    Vector(Apple, Strawberry, Pear, Raspberry, Watermelon, Mango, Banana),
    Vector(Apple, Strawberry, Watermelon, Raspberry, Pear, Mango, Banana),
    Vector(Apple, Strawberry, Mango, Raspberry, Watermelon, Pear, Banana),
    Vector(Apple, Watermelon, Banana, Pear, Strawberry, Mango, Raspberry), // 9
    Vector(Banana, Apple, Strawberry, Mango, Raspberry, Watermelon, Pear),
    Vector(Banana, Apple, Raspberry, Mango, Watermelon, Pear, Strawberry),
    Vector(Banana, Apple, Watermelon, Raspberry, Pear, Strawberry, Mango),
    Vector(Banana, Raspberry, Pear, Mango, Strawberry, Apple, Watermelon),
    Vector(Banana, Strawberry, Apple, Watermelon, Raspberry, Pear, Mango),
    Vector(Banana, Watermelon, Apple, Mango, Raspberry, Strawberry, Pear), // 6
    Vector(Pear, Apple, Banana, Mango, Watermelon, Raspberry, Strawberry),
    Vector(Pear, Banana, Apple, Mango, Strawberry, Watermelon, Raspberry),
    Vector(Pear, Raspberry, Banana, Strawberry, Watermelon, Apple, Mango),
    Vector(Pear, Raspberry, Strawberry, Mango, Watermelon, Apple, Banana),
    Vector(Pear, Strawberry, Raspberry, Banana, Apple, Mango, Watermelon),
    Vector(Pear, Strawberry, Apple, Mango, Watermelon, Banana, Raspberry),
    Vector(Pear, Strawberry, Raspberry, Banana, Mango, Apple, Watermelon),
    Vector(Pear, Watermelon, Banana, Mango, Strawberry, Raspberry, Apple),
    Vector(Pear, Watermelon, Mango, Apple, Strawberry, Banana, Raspberry), // 8
    Vector(Mango, Apple, Pear, Strawberry, Banana, Raspberry, Watermelon),
    Vector(Mango, Apple, Watermelon, Raspberry, Pear, Strawberry, Banana),
    Vector(Mango, Apple, Pear, Raspberry, Watermelon, Banana, Strawberry),
    Vector(Mango, Apple, Watermelon, Raspberry, Strawberry, Pear, Banana),
    Vector(Mango, Banana, Apple, Pear, Watermelon, Strawberry, Raspberry),
    Vector(Mango, Banana, Strawberry, Pear, Apple, Raspberry, Watermelon),
    Vector(Mango, Pear, Raspberry, Strawberry, Watermelon, Banana, Apple),
    Vector(Mango, Watermelon, Pear, Raspberry, Apple, Banana, Strawberry),
    Vector(Mango, Watermelon, Pear, Banana, Strawberry, Raspberry, Apple), // 8
    Vector(Raspberry, Banana, Watermelon, Strawberry, Pear, Apple, Mango),
    Vector(Raspberry, Mango, Pear, Watermelon, Banana, Apple, Strawberry),
    Vector(Raspberry, Mango, Apple, Pear, Banana, Watermelon, Strawberry),
    Vector(Raspberry, Mango, Banana, Apple, Watermelon, Strawberry, Pear),
    Vector(Raspberry, Strawberry, Apple, Pear, Watermelon, Mango, Banana),
    Vector(Raspberry, Watermelon, Strawberry, Apple, Banana, Pear, Mango),
    Vector(Raspberry, Watermelon, Pear, Apple, Banana, Strawberry, Mango), // 6
    Vector(Strawberry, Apple, Pear, Watermelon, Mango, Raspberry, Banana),
    Vector(Strawberry, Banana, Apple, Mango, Watermelon, Raspberry, Pear),
    Vector(Strawberry, Banana, Mango, Pear, Apple, Watermelon, Raspberry),
    Vector(Strawberry, Raspberry, Apple, Banana, Mango, Pear, Watermelon),
    Vector(Strawberry, Watermelon, Raspberry, Mango, Apple, Pear, Banana), // 5
    Vector(Watermelon, Apple, Raspberry, Mango, Banana, Pear, Strawberry),
    Vector(Watermelon, Pear, Apple, Banana, Raspberry, Mango, Strawberry),
    Vector(Watermelon, Pear, Apple, Mango, Raspberry, Banana, Strawberry),
    Vector(Watermelon, Mango, Pear, Raspberry, Apple, Banana, Strawberry), // 4
  )

  val withOneIneligibleCandidate: CountFixture =
    withOneRemainingCandidate.copy(ineligibleCandidates = Set(Apple))

  val withATie: CountFixture =
    withOneRemainingCandidate.afterEditingBallots { ballots =>
      ballots.tail :+ Vector(Watermelon, Apple, Raspberry, Mango, Banana, Pear, Strawberry)
    }

  val withInvalidIneligibleCandidates: CountFixture =
    withOneRemainingCandidate.copy(ineligibleCandidates = Set(Peach))

  val withFourCandidates: CountFixture = CountFixture(
    allBallots = Vector(
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Pear, Banana, Strawberry),
      Vector[Fruit](Apple, Banana, Strawberry, Pear),
      Vector[Fruit](Banana, Pear),
    ),
    candidates = Set[Fruit](
      Apple,
      Banana,
      Pear,
      Strawberry,
    ),
  )

}
