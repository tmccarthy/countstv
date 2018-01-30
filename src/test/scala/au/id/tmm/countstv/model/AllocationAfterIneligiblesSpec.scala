package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class AllocationAfterIneligiblesSpec extends ImprovedFlatSpec {

  private val testAllocationAfterIneligibles = AllocationAfterIneligibles(
    candidateStatuses = CandidateStatuses[Fruit](
      Apple -> Remaining,
      Banana -> Remaining,
      Pear -> Remaining,
      Strawberry -> Remaining,
    ),
    candidateVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map[Fruit, VoteCount](
        Apple -> VoteCount(40, 40),
        Banana -> VoteCount(30, 30),
        Pear -> VoteCount(20, 20),
        Strawberry -> VoteCount(10, 10),
      ),
      exhausted = VoteCount.zero,
      roundingError = VoteCount.zero,
    ),
    transfersDueToIneligibles = Map.empty[Fruit, CandidateVoteCounts[Fruit]],
  )

  "an allocation after ineligibles" should "have a count of '1'" in {
    assert(testAllocationAfterIneligibles.count === 1)
  }
}
