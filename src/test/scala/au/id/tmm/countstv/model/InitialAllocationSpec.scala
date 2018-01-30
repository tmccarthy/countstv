package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class InitialAllocationSpec extends ImprovedFlatSpec {

  private val testInitialAllocation = InitialAllocation(
    candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Remaining,
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Ineligible,
      Strawberry -> CandidateStatus.Ineligible,
    ),
    candidateVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(32, 42d),
        Banana -> VoteCount(32, 42d),
        Pear -> VoteCount(32, 42d),
        Strawberry -> VoteCount(32, 42d),
      ),
      exhausted = VoteCount(0, 0d),
      roundingError = VoteCount(0, 0d),
    )
  )

  "an initial allocation" should "have a list of candidate statuses" in {
    assert(testInitialAllocation.candidateStatuses === testInitialAllocation.candidateStatuses)
  }

  it should "have all candidates in the 'Remaining' or 'Ineligible' statuses" in {
    intercept[IllegalArgumentException]{
      testInitialAllocation.copy(
        candidateStatuses = CandidateStatuses[Fruit](
          Apple -> CandidateStatus.Remaining,
          Banana -> CandidateStatus.Ineligible,
          Pear -> CandidateStatus.Remaining,
          Strawberry -> CandidateStatus.Elected(ordinalElected = 0, electedAtCount = 1),
        ),
      )
    }
  }

  it should "have vote counts for each candidate" in {
    assert(testInitialAllocation.candidateVoteCounts === testInitialAllocation.candidateVoteCounts)
  }

  it should "have a count of '0'" in {
    assert(testInitialAllocation.count === 0)
  }
}
