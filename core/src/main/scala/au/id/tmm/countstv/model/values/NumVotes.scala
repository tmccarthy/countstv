package au.id.tmm.countstv.model.values

/**
  * A count of a number of votes.
  */
final case class NumVotes(asDouble: Double) extends AnyVal {

  def + (that: NumVotes): NumVotes = NumVotes(this.asDouble + that.asDouble)

  def - (that: NumVotes): NumVotes = NumVotes(this.asDouble - that.asDouble)

  def >(that: NumVotes): Boolean = this.asDouble > that.asDouble
  def >=(that: NumVotes): Boolean = this.asDouble >= that.asDouble
  def <=(that: NumVotes): Boolean = this.asDouble <= that.asDouble
  def <(that: NumVotes): Boolean = this.asDouble < that.asDouble

  def /(that: NumPapers): TransferValue = TransferValue(this.asDouble / that.asLong.toDouble)

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
      // Note that the behavior of rounding negative numbers to the adjacent int closest to zero is intentional here,
      // as opposed to a genuine floor operation
      NumVotes(asDouble.toLong)
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
