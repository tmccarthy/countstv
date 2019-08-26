package au.id.tmm.countstv.model.countsteps

import au.id.tmm.countstv.model.values.Count

import scala.runtime.ScalaRunTime

sealed trait CountSteps[C] extends Iterable[CountStep[C]] with PartialFunction[Count, CountStep[C]] with Product {
  def initialAllocation: InitialAllocation[C]

  override def head: CountStep[C] = initialAllocation

  override def last: CountStep[C]

  override def iterator: Iterator[CountStep[C]]

  override def hasDefiniteSize: Boolean = true

  override def size: Int

  override def isEmpty: Boolean = false

  override def isDefinedAt(count: Count): Boolean

  override def apply(count: Count): CountStep[C]

  def counts: Iterator[Count] = (0 until size).iterator.map(Count(_))

  def truncateAfter(count: Count): CountSteps[C]

  override def toString(): String = ScalaRunTime._toString(this)
}

object CountSteps {
  final case class Initial[C](initialAllocation: InitialAllocation[C]) extends CountSteps[C] {
    override def last: InitialAllocation[C] = initialAllocation

    override def iterator: Iterator[CountStep[C]] = Iterator(initialAllocation)

    override def size: Int = 1

    override def isDefinedAt(count: Count): Boolean = count == Count.ofInitialAllocation

    override def apply(count: Count): CountStep[C] = count match {
      case Count.ofInitialAllocation => initialAllocation
      case _                         => throw new IndexOutOfBoundsException(count.asInt.toString)
    }

    override def truncateAfter(count: Count): CountSteps[C] = this

    def append(allocationAfterIneligibles: AllocationAfterIneligibles[C]): AfterIneligibleHandling[C] =
      AfterIneligibleHandling(initialAllocation, allocationAfterIneligibles)
  }

  sealed trait AllowingAppending[C] extends CountSteps[C] {
    def append(distributionCountStep: DistributionPhaseCountStep[C]): DuringDistributions[C]
  }

  final case class AfterIneligibleHandling[C](
    initialAllocation: InitialAllocation[C],
    allocationAfterIneligibles: AllocationAfterIneligibles[C],
  ) extends AllowingAppending[C] {
    override def last: AllocationAfterIneligibles[C] = allocationAfterIneligibles

    override def iterator: Iterator[CountStep[C]] = Iterator(initialAllocation, allocationAfterIneligibles)

    override def size: Int = numStepsBeforeDistributionSteps

    override def isDefinedAt(count: Count): Boolean =
      count >= Count.ofInitialAllocation && count <= Count.ofIneligibleCandidateHandling

    override def apply(count: Count): CountStep[C] = count match {
      case Count.ofInitialAllocation           => initialAllocation
      case Count.ofIneligibleCandidateHandling => allocationAfterIneligibles
      case _                                   => throw new IndexOutOfBoundsException(count.asInt.toString)
    }

    override def truncateAfter(count: Count): CountSteps[C] = count match {
      case Count.ofInitialAllocation => Initial(initialAllocation)
      case _                         => this
    }

    override def append(distributionCountStep: DistributionPhaseCountStep[C]): DuringDistributions[C] =
      DuringDistributions(initialAllocation, allocationAfterIneligibles, List(distributionCountStep))
  }

  final case class DuringDistributions[C](
    initialAllocation: InitialAllocation[C],
    allocationAfterIneligibles: AllocationAfterIneligibles[C],
    distributionCountSteps: List[DistributionPhaseCountStep[C]],
  ) extends AllowingAppending[C] {
    require(distributionCountSteps.nonEmpty)

    private val lastCount: Count = Count(distributionCountSteps.size + 1)

    override def last: DistributionPhaseCountStep[C] = distributionCountSteps.last

    override def iterator: Iterator[CountStep[C]] =
      Iterator(initialAllocation, allocationAfterIneligibles) ++ distributionCountSteps.iterator

    override def size: Int = distributionCountSteps.size + numStepsBeforeDistributionSteps

    override def isDefinedAt(count: Count): Boolean = count >= Count.ofInitialAllocation && count <= lastCount

    override def apply(count: Count): CountStep[C] = count match {
      case Count.ofInitialAllocation           => initialAllocation
      case Count.ofIneligibleCandidateHandling => allocationAfterIneligibles
      case c if c <= lastCount                 => distributionCountSteps(c.asInt - numStepsBeforeDistributionSteps)
      case _                                   => throw new IndexOutOfBoundsException(count.asInt.toString)
    }

    override def truncateAfter(count: Count): CountSteps[C] = count match {
      case Count.ofInitialAllocation           => Initial(initialAllocation)
      case Count.ofIneligibleCandidateHandling => AfterIneligibleHandling(initialAllocation, allocationAfterIneligibles)
      case _: Count                            => this.copy(distributionCountSteps = this.distributionCountSteps.take(count.asInt - 1))
    }

    override def append(distributionCountStep: DistributionPhaseCountStep[C]): DuringDistributions[C] =
      this.copy(distributionCountSteps = distributionCountSteps :+ distributionCountStep)
  }

  private val numStepsBeforeDistributionSteps = 2
}
