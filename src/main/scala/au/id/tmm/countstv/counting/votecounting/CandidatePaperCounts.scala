package au.id.tmm.countstv.counting.votecounting

import au.id.tmm.countstv.model.VoteCount
import au.id.tmm.countstv.model.values.{NumPapers, TransferValue}
import au.id.tmm.countstv.utils.PerCandidateCounts

private[votecounting] final case class CandidatePaperCounts[C](
                                                                perCandidate: Map[C, NumPapers],
                                                                exhausted: NumPapers,
                                                              ) {
  def +(that: CandidatePaperCounts[C]): CandidatePaperCounts[C] =
    CandidatePaperCounts(
      perCandidate = PerCandidateCounts.combine(this.perCandidate, that.perCandidate)(_ + _),
      exhausted = this.exhausted + that.exhausted,
    )

  def -(that: CandidatePaperCounts[C]): CandidatePaperCounts[C] =
    CandidatePaperCounts(
      perCandidate = PerCandidateCounts.combine(this.perCandidate, that.perCandidate)(_ - _),
      exhausted = this.exhausted - that.exhausted,
    )

  def *(transferValue: TransferValue): CandidateVoteCountsSansRoundingError[C] =
    CandidateVoteCountsSansRoundingError(
      perCandidate = perCandidate.mapValues(numPapers => VoteCount(numPapers, numPapers * transferValue)),
      exhausted = VoteCount(exhausted, exhausted * transferValue),
    )

  def increment(candidate: C, delta: NumPapers): CandidatePaperCounts[C] =
    this.perCandidate.get(candidate) match {
      case Some(oldValue) => this.copy(perCandidate = this.perCandidate.updated(candidate, oldValue + delta))
      case None => this
    }
}

object CandidatePaperCounts {
  def zeroForEachOf[C](candidates: Set[C]): CandidatePaperCounts[C] =
    CandidatePaperCounts(
      perCandidate = candidates.map(_ -> NumPapers(0)).toMap,
      exhausted = NumPapers(0),
    )
}