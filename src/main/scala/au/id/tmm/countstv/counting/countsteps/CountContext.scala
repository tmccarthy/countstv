package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.counting.countsteps.CountContext.CurrentDistribution
import au.id.tmm.countstv.model._
import au.id.tmm.countstv.model.countsteps.{CountStep, CountSteps}
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes, TransferValueCoefficient}

import scala.collection.immutable.Queue
import scala.collection.parallel.immutable.ParSet

/**
  * An internal representation of the full state of a count, containing everything necessary to compute the context
  * after the next count step.
  */
private[counting] final case class CountContext[C] (
                                                     numFormalPapers: NumPapers,
                                                     numVacancies: Int,

                                                     paperBundles: PaperBundles[C],
                                                     candidateStatuses: CandidateStatuses[C],
                                                     previousCountSteps: CountSteps[C],

                                                     currentDistribution: Option[CurrentDistribution[C]],
                                                   ) {

  val quota: NumVotes = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

  def mostRecentCountStep: CountStep[C] = previousCountSteps.last

  def electedCandidatesWaitingToBeDistributed: Queue[C] = {
    val electedCandidateCurrentlyBeingDistributed = currentDistribution match {
      case Some(CurrentDistribution(candidateBeingDistributed, CandidateDistributionReason.Election, _, _)) =>
        Some(candidateBeingDistributed)
      case _ => None
    }

    mostRecentCountStep
      .candidateStatuses
      .electedCandidates
      .toStream
      .filter { c =>
        !electedCandidateCurrentlyBeingDistributed.contains(c) &&
          mostRecentCountStep.candidateVoteCounts.perCandidate(c).numPapers > NumPapers(0)
      }
      .to[Queue]
  }

  lazy val previousCandidateVoteCounts: List[CandidateVoteCounts[C]] =
    previousCountSteps.toList.map(_.candidateVoteCounts)

  def allVacanciesNowFilled: Boolean = candidateStatuses.electedCandidates.size == numVacancies || (
    numVacancies > candidateStatuses.eligibleCandidates.size && candidateStatuses.electedCandidates.size == candidateStatuses.eligibleCandidates.size
    )

}

object CountContext {

  def apply[C](
                numFormalPapers: NumPapers,
                numVacancies: Int,

                paperBundles: PaperBundles[C],
                previousCountSteps: CountSteps[C],

                currentDistribution: Option[CurrentDistribution[C]],
              ): CountContext[C] = {
    new CountContext(
      numFormalPapers,
      numVacancies,
      paperBundles,
      previousCountSteps.last.candidateStatuses,
      previousCountSteps,
      currentDistribution
    )
  }

  final case class CurrentDistribution[C](
                                           candidateBeingDistributed: C,
                                           distributionReason: CandidateDistributionReason,
                                           bundlesToDistribute: Queue[ParSet[AssignedPaperBundle[C]]],
                                           transferValueCoefficient: TransferValueCoefficient,
                                         ) {
    require(bundlesToDistribute.nonEmpty)
  }

}