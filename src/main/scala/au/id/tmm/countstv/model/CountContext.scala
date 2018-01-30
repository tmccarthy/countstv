package au.id.tmm.countstv.model

import au.id.tmm.countstv.counting.{PaperBundle, QuotaComputation}

import scala.collection.immutable.{Bag, Queue}

final case class CountContext[C] (
                                   numFormalPapers: Long,
                                   numVacancies: Int,

                                   paperBundles: Bag[PaperBundle[C]],
                                   mostRecentCountStep: CountStep[C],

                                   candidateCurrentlyBeingExcluded: Option[C],
                                   candidateCurrentlyBeingElected: Option[C],

                                   paperBundlesToBeDistributed: Bag[PaperBundle[C]],
                                 ) {
  val quota: Long = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

  def electedCandidatesToBeDistributed: Queue[C] = {
    mostRecentCountStep
      .candidateStatuses
      .electedCandidates
      .filterNot(candidateCurrentlyBeingElected.contains)
      .to[Queue]
  }
}
