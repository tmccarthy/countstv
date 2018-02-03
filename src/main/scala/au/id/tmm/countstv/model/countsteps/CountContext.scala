package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.model.countsteps.CountContext.CurrentDistribution
import au.id.tmm.countstv.model.{AssignedPaperBundle, CandidateDistributionReason}

import scala.collection.immutable.{Bag, Queue}

final case class CountContext[C] (
                                   numFormalPapers: Long,
                                   numVacancies: Int,

                                   paperBundles: PaperBundles[C],
                                   mostRecentCountStep: CountStep[C],

                                   currentDistribution: Option[CurrentDistribution[C]],
                                 ) {
  val quota: Long = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

  def electedCandidatesToBeDistributed: Queue[C] = {
    val electedCandidateCurrentlyBeingDistributed = currentDistribution match {
      case Some(CurrentDistribution(candidateBeingDistributed, CandidateDistributionReason.Election, _)) =>
        Some(candidateBeingDistributed)
      case _ => None
    }

    mostRecentCountStep
      .candidateStatuses
      .electedCandidates
      .toStream
      .filterNot(electedCandidateCurrentlyBeingDistributed.contains)
      .to[Queue]
  }
}

object CountContext {

  final case class CurrentDistribution[C](
                                           candidateBeingDistributed: C,
                                           distributionReason: CandidateDistributionReason,
                                           bundlesToDistribute: Queue[Bag[AssignedPaperBundle[C]]],
                                         ) {
    require(bundlesToDistribute.nonEmpty)
  }

}