package au.id.tmm.countstv.model.values

/**
  * A count of a number of votes.
  */
final case class NumVotes(asLong: Long) extends AnyVal {

  def + (that: NumVotes): NumVotes = NumVotes(this.asLong + that.asLong)

  def - (that: NumVotes): NumVotes = NumVotes(this.asLong - that.asLong)

  def >(that: NumVotes): Boolean = this.asLong > that.asLong
  def >=(that: NumVotes): Boolean = this.asLong >= that.asLong
  def <=(that: NumVotes): Boolean = this.asLong <= that.asLong
  def <(that: NumVotes): Boolean = this.asLong < that.asLong

  def /(that: NumPapers): TransferValue = TransferValue(this.asLong.toDouble / that.asLong.toDouble)

}

object NumVotes {

  // This method has a weird implementation to try to avoid some of the most glaring floating point errors.
  // TODO investigate using rational for transfer values
  def byRoundingDown(asDouble: Double): NumVotes = {
    val nearestInt = math.round(asDouble)

    val distanceToNearestInt = math.abs(nearestInt - asDouble)

    if (distanceToNearestInt <= 1e-9) {
      NumVotes(nearestInt.toLong)
    } else {
      NumVotes(math.floor(asDouble).toLong)
    }
  }

  implicit val ordering: Ordering[NumVotes] = (x: NumVotes, y: NumVotes) => {
    if (x > y) {
      1
    } else if (y > x) {
      -1
    } else {
      0
    }
  }
}
