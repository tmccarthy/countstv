package au.id.tmm.countstv.model.values

final case class Count(countNumber: Int) extends AnyVal {

  def increment: Count = Count(countNumber + 1)

}

object Count {
  val ofInitialAllocation: Count = Count(0)

  val ofIneligibleCandidateHandling: Count = Count(1)
}
