package au.id.tmm.countstv.model.values

/**
  * An ordinal number.
  */
final case class Ordinal(asInt: Int) extends AnyVal {

  def >(that: Ordinal): Boolean = this.asInt > that.asInt
  def >=(that: Ordinal): Boolean = this.asInt >= that.asInt
  def <=(that: Ordinal): Boolean = this.asInt <= that.asInt
  def <(that: Ordinal): Boolean = this.asInt < that.asInt

}

object Ordinal {
  implicit val ordering: Ordering[Ordinal] = (x: Ordinal, y: Ordinal) => {
    if (x > y) {
      1
    } else if (y > x) {
      -1
    } else {
      0
    }
  }

  def ofNextAdditionTo[A](list: Iterable[A]): Ordinal = Ordinal(list.size)

  val first = Ordinal(0)
  val second = Ordinal(1)
  val third = Ordinal(2)
  val fourth = Ordinal(3)
  val fifth  = Ordinal(4)
  val sixth  = Ordinal(5)
  val seventh  = Ordinal(6)
  val eighth  = Ordinal(7)
  val ninth  = Ordinal(8)
  val tenth  = Ordinal(9)
}