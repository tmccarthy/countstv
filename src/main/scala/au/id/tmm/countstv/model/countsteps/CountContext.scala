package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.PaperBundles
import au.id.tmm.countstv.counting.QuotaComputation
import au.id.tmm.countstv.model.countsteps.CountContext.CurrentDistribution
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes, TransferValueCoefficient}
import au.id.tmm.countstv.model.{AssignedPaperBundle, CandidateDistributionReason, CandidateStatuses, CandidateVoteCounts}

import scala.collection.immutable.{Bag, Queue}

final case class CountContext[C] (
                                   numFormalPapers: NumPapers,
                                   numVacancies: Int,

                                   paperBundles: PaperBundles[C],
                                   candidateStatuses: CandidateStatuses[C],
                                   previousCountSteps: List[CountStep[C]],

                                   currentDistribution: Option[CurrentDistribution[C]],
                                 ) {

  require(previousCountSteps.nonEmpty)

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

  lazy val previousCandidateVoteCounts: List[CandidateVoteCounts[C]] = previousCountSteps.map(_.candidateVoteCounts)

  def allVacanciesNowFilled: Boolean = candidateStatuses.electedCandidates.size == numVacancies || (
    numVacancies > candidateStatuses.allCandidates.size && candidateStatuses.electedCandidates.size == candidateStatuses.allCandidates.size
  )

}

object CountContext {

  def apply[C](
                numFormalPapers: NumPapers,
                numVacancies: Int,

                paperBundles: PaperBundles[C],
                previousCountSteps: List[CountStep[C]],

                currentDistribution: Option[CurrentDistribution[C]],
              ): CountContext[C] = {
    require(previousCountSteps.nonEmpty)

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
                                           bundlesToDistribute: Queue[Bag[AssignedPaperBundle[C]]],
                                           transferValueCoefficient: TransferValueCoefficient,
                                         ) {
    require(bundlesToDistribute.nonEmpty)
  }

}