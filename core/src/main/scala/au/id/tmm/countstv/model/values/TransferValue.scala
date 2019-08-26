package au.id.tmm.countstv.model.values

/**
  * The coefficient applied to a count of papers in order to get the value of a paper or paper bundle
  */
final case class TransferValue(factor: Double) extends AnyVal {

  def >(that: TransferValue): Boolean  = this.factor > that.factor
  def >=(that: TransferValue): Boolean = this.factor >= that.factor
  def <=(that: TransferValue): Boolean = this.factor <= that.factor
  def <(that: TransferValue): Boolean  = this.factor < that.factor

}

object TransferValue {

  implicit val ordering: Ordering[TransferValue] = { (x, y) =>
    if (x > y) {
      1
    } else if (y > x) {
      -1
    } else {
      0
    }
  }

}
