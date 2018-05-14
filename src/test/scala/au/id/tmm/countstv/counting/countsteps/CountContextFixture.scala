package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.counting.{AssignedPaperBundle, PaperBundle}
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.countsteps.{AllocationAfterIneligibles, CountSteps, InitialAllocation}
import au.id.tmm.countstv.model.values.{NumPapers, TransferValue}
import au.id.tmm.countstv.model.{CandidateStatuses, CandidateVoteCounts, PreferenceTree, VoteCount}

import scala.collection.parallel.immutable.ParSet

object CountContextFixture {

  val initialAllocation: InitialAllocation[Fruit] = InitialAllocation[Fruit](
    candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Remaining,
      Banana -> Remaining,
      Pear -> Remaining,
      Strawberry -> Remaining,
    ),
    candidateVoteCounts = CandidateVoteCounts(
      perCandidate = Map[Fruit, VoteCount](
        Apple -> VoteCount(20),
        Banana -> VoteCount(10),
        Pear -> VoteCount(6),
        Strawberry -> VoteCount(4),
      ),
      exhausted = VoteCount.zero,
      roundingError = VoteCount.zero,
    ),
  )

  val allocationAfterIneligibles: AllocationAfterIneligibles[Fruit] = AllocationAfterIneligibles[Fruit](
    candidateStatuses = initialAllocation.candidateStatuses,
    candidateVoteCounts = initialAllocation.candidateVoteCounts,
    transfersDueToIneligibles = Map.empty[Fruit, CandidateVoteCounts[Fruit]],
  )

  val testContext = CountContext(
    numFormalPapers = NumPapers(40),
    numVacancies = 2,
    paperBundles = ParSet.empty[PaperBundle[Fruit]],
    previousCountSteps = CountSteps.AfterIneligibleHandling(initialAllocation, allocationAfterIneligibles),
  )

  val testPreferenceTree: PreferenceTree[Fruit] = PreferenceTree.from[Fruit](
    Vector(Apple, Pear, Banana, Strawberry),
    Vector(Apple, Banana, Strawberry, Pear),
    Vector(Banana, Pear),
  )

  val testBundle = AssignedPaperBundle(
    transferValue = TransferValue(1.0d),
    testPreferenceTree.childFor(Apple).get,
    PaperBundle.Origin.InitialAllocation,
  )
}
