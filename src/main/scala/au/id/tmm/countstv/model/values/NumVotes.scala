package au.id.tmm.countstv.model.values

// TODO there should be a way to make this an integral type
final case class NumVotes(asDouble: Double) extends AnyVal {

  def roundedDown: NumVotes = NumVotes(math.floor(asDouble))

  def + (that: NumVotes): NumVotes = NumVotes(this.asDouble + that.asDouble)

  def - (that: NumVotes): NumVotes = NumVotes(this.asDouble - that.asDouble)

  def >(that: NumVotes): Boolean = this.asDouble > that.asDouble
  def >=(that: NumVotes): Boolean = this.asDouble >= that.asDouble
  def <=(that: NumVotes): Boolean = this.asDouble <= that.asDouble
  def <(that: NumVotes): Boolean = this.asDouble < that.asDouble

}

object NumVotes {
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
