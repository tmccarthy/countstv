package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.votecounting.FullCountVoteCounting
import au.id.tmm.countstv.counting.{QuotaComputation, RootPaperBundle}
import au.id.tmm.countstv.model.countsteps.{CountSteps, InitialAllocation}
import au.id.tmm.countstv.model.{CandidateStatus, CandidateStatuses}
import au.id.tmm.countstv.rules.RoundingRules

private[counting] object InitialAllocationComputation {

  /**
    * Computes the initial context by distributing papers to their first preferences, including to ineligible
    * candidates.
    */
  def computeInitialContext[C](
    allCandidates: Set[C],
    ineligibleCandidates: Set[C],
    numVacancies: Int,
    rootPaperBundle: RootPaperBundle[C],
  )(implicit
    roundingRules: RoundingRules,
  ): CountContext.Initial[C] = {
    val numFormalPapers = rootPaperBundle.numPapers
    val quota           = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

    val firstSetOfPaperBundles = rootPaperBundle.distribute

    val initialCandidateStatuses = CandidateStatuses(
      allCandidates.map { candidate =>
        if (ineligibleCandidates contains candidate) {
          candidate -> CandidateStatus.Ineligible
        } else {
          candidate -> CandidateStatus.Remaining
        }
      }.toMap,
    )

    CountContext.Initial[C](
      numFormalPapers = numFormalPapers,
      numVacancies = numVacancies,
      quota = quota,
      paperBundles = firstSetOfPaperBundles,
      previousCountSteps = CountSteps.Initial(
        InitialAllocation(
          candidateStatuses = initialCandidateStatuses,
          candidateVoteCounts = FullCountVoteCounting.performFullRecount(
            initialNumPapers = rootPaperBundle.numPapers,
            quota = quota,
            candidateStatuses = initialCandidateStatuses,
            paperBundles = firstSetOfPaperBundles,
          ),
        ),
      ),
    )

  }
}
