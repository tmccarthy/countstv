package au.id.tmm.countstv.model

import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.model.CountContext.CurrentDistribution

import scala.collection.immutable.{Bag, Queue}

final case class CountContext[C] (
                                   numFormalPapers: Long,
                                   numVacancies: Int,

                                   paperBundles: Bag[PaperBundle[C]],
                                   mostRecentCountStep: CountStep[C],

                                   currentDistribution: CurrentDistribution[C],

                                   paperBundlesToBeDistributed: Bag[PaperBundle[C]],
                                 ) {
  val quota: Long = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

  def electedCandidatesToBeDistributed: Queue[C] = {
    val electedCandidateCurrentlyBeingDistributed = currentDistribution match {
      case CurrentDistribution.ElectedCandidate(candidateBeingDistributed) => Some(candidateBeingDistributed)
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

  sealed trait CurrentDistribution[+C]

  object CurrentDistribution {
    final case class ExcludedCandidate[C](candidateBeingDistributed: C) extends CurrentDistribution[C]
    final case class ElectedCandidate[C](candidateBeingDistributed: C) extends CurrentDistribution[C]
    case object NoDistribution extends CurrentDistribution[Nothing]
  }

}