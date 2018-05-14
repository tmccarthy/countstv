package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.{AssignedPaperBundle, PaperBundles, QuotaComputation}
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountStep, CountSteps}
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes, TransferValueCoefficient}

import scala.collection.immutable.Queue
import scala.collection.parallel.immutable.ParSet

/**
  * An internal representation of the full state of a count, containing everything necessary to compute the context
  * after the next count step.
  */
private[counting] final case class CountContext[C, +T_COUNT_STEPS <: CountSteps[C]] (
                                                                                     numFormalPapers: NumPapers,
                                                                                     numVacancies: Int,

                                                                                     paperBundles: PaperBundles[C],
                                                                                     candidateStatuses: CandidateStatuses[C],
                                                                                     previousCountSteps: T_COUNT_STEPS,
                                                                                   ) {

  val quota: NumVotes = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

  def mostRecentCountStep: CountStep[C] = previousCountSteps.last

  lazy val previousCandidateVoteCounts: List[CandidateVoteCounts[C]] =
    previousCountSteps.toList.map(_.candidateVoteCounts)

  def allVacanciesNowFilled: Boolean = candidateStatuses.electedCandidates.size == numVacancies || (
    numVacancies > candidateStatuses.eligibleCandidates.size && candidateStatuses.electedCandidates.size == candidateStatuses.eligibleCandidates.size
    )

}

object CountContext {

  def apply[C, T_COUNT_STEPS <: CountSteps[C]](
                                                numFormalPapers: NumPapers,
                                                numVacancies: Int,

                                                paperBundles: PaperBundles[C],
                                                previousCountSteps: T_COUNT_STEPS,
                                              ): CountContext[C, T_COUNT_STEPS] = {
    new CountContext(
      numFormalPapers,
      numVacancies,
      paperBundles,
      previousCountSteps.last.candidateStatuses,
      previousCountSteps,
    )
  }

  final case class CurrentDistribution[C](
                                           candidateBeingDistributed: C,
                                           distributionReason: CandidateDistributionReason,
                                           bundlesToDistribute: Queue[ParSet[AssignedPaperBundle[C]]],
                                           transferValueCoefficient: TransferValueCoefficient,
                                         ) {
    require(bundlesToDistribute.nonEmpty) // TODO what happens if a candidate genuinely has no votes?
  }

}