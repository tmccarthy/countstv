package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.values.{Count, NumPapers, NumVotes, Ordinal}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, CandidateVoteCounts, VoteCount}
import org.scalatest.FlatSpec

class InitialAllocationSpec extends FlatSpec {

  private val testInitialAllocation = InitialAllocation(
    candidateStatuses = CandidateStatuses[Fruit](
      Apple -> CandidateStatus.Remaining,
      Banana -> CandidateStatus.Remaining,
      Pear -> CandidateStatus.Ineligible,
      Strawberry -> CandidateStatus.Ineligible,
    ),
    candidateVoteCounts = CandidateVoteCounts[Fruit](
      perCandidate = Map(
        Apple -> VoteCount(NumPapers(32), NumVotes(42)),
        Banana -> VoteCount(NumPapers(32), NumVotes(42)),
        Pear -> VoteCount(NumPapers(32), NumVotes(42)),
        Strawberry -> VoteCount(NumPapers(32), NumVotes(42)),
      ),
      exhausted = VoteCount(NumPapers(0), NumVotes(0)),
      roundingError = VoteCount(NumPapers(0), NumVotes(0)),
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
          Strawberry -> CandidateStatus.Elected(Ordinal.first, electedAtCount = Count(1)),
        ),
      )
    }
  }

  it should "have vote counts for each candidate" in {
    assert(testInitialAllocation.candidateVoteCounts === testInitialAllocation.candidateVoteCounts)
  }

  it should "have a count of '0'" in {
    assert(testInitialAllocation.count === Count(0))
  }
}
