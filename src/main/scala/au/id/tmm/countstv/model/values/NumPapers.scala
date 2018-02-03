package au.id.tmm.countstv.model.values

final case class NumPapers(asLong: Long) extends AnyVal {
  def + (that: NumPapers): NumPapers = NumPapers(this.asLong + that.asLong)

  def - (that: NumPapers): NumPapers = NumPapers(this.asLong - that.asLong)

  def >(that: NumPapers): Boolean = this.asLong > that.asLong
  def >=(that: NumPapers): Boolean = this.asLong >= that.asLong
  def <=(that: NumPapers): Boolean = this.asLong <= that.asLong
  def <(that: NumPapers): Boolean = this.asLong < that.asLong

}

object NumPapers {
  implicit val ordering: Ordering[NumPapers] = (x: NumPapers, y: NumPapers) => {
    if (x > y) {
      1
    } else if (y > x) {
      -1
    } else {
      0
    }
  }
}
