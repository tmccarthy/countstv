package au.id.tmm.countstv.model.countsteps

final case class CountSteps[C](
                                initialAllocation: InitialAllocation[C],
                                allocationAfterIneligibles: Option[AllocationAfterIneligibles[C]],
                                distributionCountSteps: List[DistributionCountStep[C]],
                              ) {

  def last: CountStep[C] =
    distributionCountSteps.lastOption orElse allocationAfterIneligibles getOrElse initialAllocation

  def asList: List[CountStep[C]] = List(initialAllocation) ++ allocationAfterIneligibles ++ distributionCountSteps

  def append(distributionCountStep: DistributionCountStep[C]): CountSteps[C] =
    this.copy(distributionCountSteps = distributionCountSteps :+ distributionCountStep)
}
