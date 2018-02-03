package au.id.tmm.countstv.model.values

/**
  * The amount by which a [[TransferValue]] is modified during a count step
  */
final case class TransferValueCoefficient(coefficient: Double) {

  def * (transferValue: TransferValue): TransferValue = TransferValue(transferValue.factor * coefficient)

}

object TransferValueCoefficient {
  def compute(numVotes: NumVotes, quota: NumVotes): TransferValueCoefficient = {
    val surplus = numVotes - quota

    TransferValueCoefficient(surplus.asDouble / numVotes.asDouble)
  }
}
