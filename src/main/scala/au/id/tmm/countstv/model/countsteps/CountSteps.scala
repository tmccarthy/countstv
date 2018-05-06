package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.model.countsteps.CountSteps.numStepsBeforeDistributionSteps
import au.id.tmm.countstv.model.values.Count

final case class CountSteps[C](
                                initialAllocation: InitialAllocation[C],
                                allocationAfterIneligibles: Option[AllocationAfterIneligibles[C]],
                                distributionCountSteps: List[DistributionCountStep[C]],
                              )
  extends Iterable[CountStep[C]]
    with PartialFunction[Count, CountStep[C]] {

  ensureValid()

  private def ensureValid(): Unit = {
    if (distributionCountSteps.nonEmpty) {
      require(allocationAfterIneligibles.nonEmpty)
    }
  }

  override def head: CountStep[C] = initialAllocation

  override def last: CountStep[C] =
    distributionCountSteps.lastOption orElse allocationAfterIneligibles getOrElse initialAllocation

  def secondLast: Option[CountStep[C]] = {
    size match {
      case 0 | 1 => None
      case 2 => Some(initialAllocation)
      case 3 => allocationAfterIneligibles
      case _ => distributionCountSteps.init.takeRight(2).headOption
    }
  }

  def append(distributionCountStep: DistributionCountStep[C]): CountSteps[C] =
    this.copy(distributionCountSteps = distributionCountSteps :+ distributionCountStep)

  override def iterator: Iterator[CountStep[C]] =
    Iterator(initialAllocation) ++ allocationAfterIneligibles ++ distributionCountSteps

  override def size: Int = 1 + allocationAfterIneligibles.size + distributionCountSteps.size

  override def isEmpty: Boolean = false

  override def isDefinedAt(count: Count): Boolean = count.asInt >= 0 && count.asInt <= size - 1

  override def apply(count: Count): CountStep[C] = {
    count match {
      case Count.ofInitialAllocation => initialAllocation
      case Count.ofIneligibleCandidateHandling =>
        allocationAfterIneligibles.getOrElse(throw new IndexOutOfBoundsException(count.asInt.toString))
      case _ => distributionCountSteps(count.asInt - numStepsBeforeDistributionSteps)
    }
  }

  def truncateAfter(count: Count): CountSteps[C] = {
    count match {
      case Count.ofInitialAllocation => this.copy(allocationAfterIneligibles = None, distributionCountSteps = Nil)
      case Count.ofIneligibleCandidateHandling => this.copy(distributionCountSteps = Nil)
      case _ => this.copy(distributionCountSteps = this.distributionCountSteps.take(count.asInt - 1))
    }
  }
}

object CountSteps {
  private val numStepsBeforeDistributionSteps = 2
}
