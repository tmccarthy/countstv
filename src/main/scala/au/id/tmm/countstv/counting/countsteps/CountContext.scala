package au.id.tmm.countstv.counting.countsteps

import au.id.tmm.countstv.counting.{CountAction, PaperBundles, QuotaComputation}
import au.id.tmm.countstv.model.countsteps._
import au.id.tmm.countstv.model.values.{NumPapers, NumVotes}

private[counting] sealed trait CountContext[C] {

  def numFormalPapers: NumPapers
  def numVacancies: Int
  lazy val quota: NumVotes = QuotaComputation.computeQuota(numVacancies, numFormalPapers)

  def paperBundles: PaperBundles[C]

  def previousCountSteps: CountSteps[C]
  def mostRecentCountStep: CountStep[C]

  def nextAction: CountAction[C]

}

private[counting] object CountContext {

  final case class Initial[C](
                               numFormalPapers: NumPapers,
                               numVacancies: Int,

                               paperBundles: PaperBundles[C],

                               previousCountSteps: CountSteps.Initial[C],
                             ) extends CountContext[C] {
    override def mostRecentCountStep: InitialAllocation[C] = previousCountSteps.last

    override def nextAction: CountAction.AllocateAwayFromIneligibles.type = CountAction.AllocateAwayFromIneligibles

    def updated(
                 newPaperBundles: PaperBundles[C],
                 newCountStep: AllocationAfterIneligibles[C],
                 nextAction: CountAction.DuringDistribution[C],
               ): AfterIneligibleHandling[C] =
      AfterIneligibleHandling(
        numFormalPapers,
        numVacancies,
        newPaperBundles,
        previousCountSteps.append(newCountStep),
        nextAction,
      )
  }

  sealed trait AllowingAppending[C] extends CountContext[C] {
    override def previousCountSteps: CountSteps.AllowingAppending[C]

    def updated(
                 newPaperBundles: PaperBundles[C],
                 newCountStep: DistributionPhaseCountStep[C],
                 nextAction: CountAction.DuringDistribution[C],
               ): CountContext.DuringDistributions[C]

    def updated(
                 newCountStep: FinalElectionCountStep[C],
                 nextAction: CountAction.NoAction.type,
               ): CountContext.Terminal[C] = Terminal(
      numFormalPapers,
      numVacancies,
      paperBundles,
      previousCountSteps.append(newCountStep),
    )

    override def nextAction: CountAction.DuringDistribution[C]
  }

  final case class AfterIneligibleHandling[C](
                                               numFormalPapers: NumPapers,
                                               numVacancies: Int,

                                               paperBundles: PaperBundles[C],

                                               previousCountSteps: CountSteps.AfterIneligibleHandling[C],

                                               override val nextAction: CountAction.DuringDistribution[C],
                                             ) extends CountContext[C] with AllowingAppending[C] {
    override def mostRecentCountStep: AllocationAfterIneligibles[C] = previousCountSteps.last

    override def updated(
                          newPaperBundles: PaperBundles[C],
                          newCountStep: DistributionPhaseCountStep[C],
                          nextAction: CountAction.DuringDistribution[C],
                        ): DuringDistributions[C] =
      DuringDistributions(
        this.numFormalPapers,
        this.numVacancies,
        newPaperBundles,
        this.previousCountSteps.append(newCountStep),
        nextAction
      )
  }

  sealed trait DistributionPhase[C] extends CountContext[C]

  final case class DuringDistributions[C](
                                           numFormalPapers: NumPapers,
                                           numVacancies: Int,

                                           paperBundles: PaperBundles[C],

                                           previousCountSteps: CountSteps.DuringDistributions[C],

                                           override val nextAction: CountAction.DuringDistribution[C],
                                         ) extends CountContext[C] with AllowingAppending[C] with DistributionPhase[C] {
    override def mostRecentCountStep: DistributionPhaseCountStep[C] = previousCountSteps.last

    override def updated(
                          newPaperBundles: PaperBundles[C],
                          newCountStep: DistributionPhaseCountStep[C],
                          nextAction: CountAction.DuringDistribution[C],
                        ): DuringDistributions[C] =
      this.copy(
        paperBundles = newPaperBundles,
        previousCountSteps = this.previousCountSteps.append(newCountStep),
        nextAction = nextAction,
      )
  }

  final case class Terminal[C](
                                numFormalPapers: NumPapers,
                                numVacancies: Int,

                                paperBundles: PaperBundles[C],

                                previousCountSteps: CountSteps.DistributionPhase[C],
                              ) extends CountContext[C] with DistributionPhase[C] {
    override def mostRecentCountStep: CountStep[C] = previousCountSteps.last

    override def nextAction: CountAction.NoAction.type = CountAction.NoAction
  }

}
