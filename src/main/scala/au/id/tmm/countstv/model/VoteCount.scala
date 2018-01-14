package au.id.tmm.countstv.model

final case class VoteCount(numPapers: Long, numVotes: Double) {

  def +(that: VoteCount): VoteCount = VoteCount(
    numPapers = this.numPapers + that.numPapers,
    numVotes = this.numVotes + that.numVotes,
  )

  def -(that: VoteCount): VoteCount = VoteCount(
    numPapers = this.numPapers - that.numPapers,
    numVotes = this.numVotes - that.numVotes,
  )

  def *(scalar: Long): VoteCount = VoteCount(
    numPapers = this.numPapers * scalar,
    numVotes = this.numVotes * scalar,
  )

}

object VoteCount {
  val empty: VoteCount = VoteCount(0, 0d)
}