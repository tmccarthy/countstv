package au.id.tmm.countstv.model.values

import au.id.tmm.countstv.rules.RoundingRules

/**
  * A count of a number of ballot papers.
  */
final case class NumPapers(asLong: Long) extends AnyVal {
  def +(that: NumPapers): NumPapers = NumPapers(this.asLong + that.asLong)

  def -(that: NumPapers): NumPapers = NumPapers(this.asLong - that.asLong)

  def >(that: NumPapers): Boolean  = this.asLong > that.asLong
  def >=(that: NumPapers): Boolean = this.asLong >= that.asLong
  def <=(that: NumPapers): Boolean = this.asLong <= that.asLong
  def <(that: NumPapers): Boolean  = this.asLong < that.asLong

  def *(transferValue: TransferValue)(implicit roundingRules: RoundingRules): NumVotes =
    if (roundingRules.roundTransferValueMultiplication) {
      NumVotes.byRoundingDown(asLong * transferValue.factor)
    } else {
      NumVotes(asLong.toDouble * transferValue.factor)
    }
}

object NumPapers {
  implicit val ordering: Ordering[NumPapers] = (x: NumPapers, y: NumPapers) => {
    if (x > y) {
      1
    } else if (y > x) {
      -1
    } else {
      0
    },
  }
}
