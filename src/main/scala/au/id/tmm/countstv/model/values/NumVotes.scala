package au.id.tmm.countstv.model.values

final case class NumVotes(asLong: Long) extends AnyVal {

  def + (that: NumVotes): NumVotes = NumVotes(this.asLong + that.asLong)

  def - (that: NumVotes): NumVotes = NumVotes(this.asLong - that.asLong)

  def >(that: NumVotes): Boolean = this.asLong > that.asLong
  def >=(that: NumVotes): Boolean = this.asLong >= that.asLong
  def <=(that: NumVotes): Boolean = this.asLong <= that.asLong
  def <(that: NumVotes): Boolean = this.asLong < that.asLong

}

object NumVotes {

  def byRoundingDown(asDouble: Double): NumVotes = NumVotes(math.floor(asDouble).toLong)

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
