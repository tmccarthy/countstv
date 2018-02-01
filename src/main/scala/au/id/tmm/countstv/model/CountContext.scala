package au.id.tmm.countstv.model

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.model.CountContext.CurrentDistribution

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
      case Some(CurrentDistribution.ElectedCandidate(candidateBeingDistributed, _)) => Some(candidateBeingDistributed)
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

  sealed trait CurrentDistribution[C] {
    def candidateBeingDistributed: C
    def bundlesToDistribute: Queue[Bag[AssignedPaperBundle[C]]]

    require(bundlesToDistribute.nonEmpty)
  }

  object CurrentDistribution {
    final case class ExcludedCandidate[C](
                                           candidateBeingDistributed: C,
                                           bundlesToDistribute: Queue[Bag[AssignedPaperBundle[C]]],
                                         ) extends CurrentDistribution[C]

    final case class ElectedCandidate[C](
                                          candidateBeingDistributed: C,
                                          bundlesToDistribute: Queue[Bag[AssignedPaperBundle[C]]],
                                        ) extends CurrentDistribution[C]
  }

}