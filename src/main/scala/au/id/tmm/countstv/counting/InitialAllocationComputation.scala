package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses, InitialAllocation}

import scala.collection.immutable.Bag

object InitialAllocationComputation {

  private val allowedCandidateStatuses: Set[CandidateStatus] =
    Set(CandidateStatus.Remaining, CandidateStatus.Ineligible)

  def computeInitialAllocation[C](
                                   candidateStatuses: CandidateStatuses[C],
                                   quota: Long,
                                   numFormalPapers: Long,
                                   paperBundles: Bag[PaperBundle[C]],
                                 ): InitialAllocation[C] = {
    require(candidateStatuses.asMap.valuesIterator.forall(allowedCandidateStatuses.contains))

    val candidateVoteCounts = VoteCounting.countVotes(
      initialNumPapers = numFormalPapers,
      quota = quota,
      candidateStatuses = candidateStatuses,
      paperBundles = paperBundles
    )

    InitialAllocation(
      candidateStatuses,
      candidateVoteCounts,
    )
  }

}
