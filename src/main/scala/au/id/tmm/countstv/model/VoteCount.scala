package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}

final case class VoteCount(numPapers: NumPapers, numVotes: NumVotes) {

  def +(that: VoteCount): VoteCount = VoteCount(
    numPapers = this.numPapers + that.numPapers,
    numVotes = this.numVotes + that.numVotes,
  )

  def -(that: VoteCount): VoteCount = VoteCount(
    numPapers = this.numPapers - that.numPapers,
    numVotes = this.numVotes - that.numVotes,
  )

}

object VoteCount {
  val zero: VoteCount = VoteCount(NumPapers(0), NumVotes(0))

  def apply(numVotesAndPapers: Long): VoteCount = VoteCount(NumPapers(numVotesAndPapers), NumVotes(numVotesAndPapers))
}