package au.id.tmm.countstv.model.values

/**
  * A count number.
  */
final case class Count(asInt: Int) extends AnyVal {

  def increment: Count = Count(asInt + 1)

  def >(that: Count): Boolean = this.asInt > that.asInt
  def >=(that: Count): Boolean = this.asInt >= that.asInt
  def <=(that: Count): Boolean = this.asInt <= that.asInt
  def <(that: Count): Boolean = this.asInt < that.asInt

}

object Count {
  val ofInitialAllocation: Count = Count(0)

  val ofIneligibleCandidateHandling: Count = Count(1)

  implicit val ordering: Ordering[Count] = (x: Count, y: Count) => {
    if (x > y) {
      1
    } else if (y > x) {
      -1
    } else {
      0
    }
  }
}
