package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.{QuotaComputation, VoteCounting}
import au.id.tmm.countstv.model._

import scala.collection.immutable.Bag

object InitialAllocationComputation {

  private val allowedCandidateStatuses: Set[CandidateStatus] =
    Set(CandidateStatus.Remaining, CandidateStatus.Ineligible)

  def computeInitialContext[C](
                                initialCandidateStatuses: CandidateStatuses[C],
                                rootPaperBundle: RootPaperBundle[C],
                                numVacancies: Int,
                              ): CountContext[C] = {
    val numFormalPapers = rootPaperBundle.numPapers
    val quota = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

    val firstSetOfPaperBundles = rootPaperBundle.distribute

    CountContext[C](
      numFormalPapers = numFormalPapers,
      numVacancies = numVacancies,
      paperBundles = firstSetOfPaperBundles,
      mostRecentCountStep = InitialAllocation(
        candidateStatuses = initialCandidateStatuses,
        candidateVoteCounts = VoteCounting.countVotes(
          initialNumPapers = rootPaperBundle.numPapers,
          quota = quota,
          candidateStatuses = initialCandidateStatuses,
          paperBundles = firstSetOfPaperBundles,
        )
      ),
      excludedCandidateBeingDistributed = None,
      electedCandidateBeingDistributed = None,
      paperBundlesToBeDistributed = Bag.empty(PaperBundle.bagConfiguration),
    )

  }
}
