package au.id.tmm.countstv.model.values

/**
  * A count number.
  */
final case class Count(asInt: Int) extends AnyVal {

  def increment: Count = Count(asInt + 1)

}

object Count {
  val ofInitialAllocation: Count = Count(0)

  val ofIneligibleCandidateHandling: Count = Count(1)
}
