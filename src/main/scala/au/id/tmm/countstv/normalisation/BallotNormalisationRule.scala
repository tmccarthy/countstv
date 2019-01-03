package au.id.tmm.countstv.normalisation

sealed trait BallotNormalisationRule

object BallotNormalisationRule {

  final case class MinimumPreferences(numPreferences: Int) extends BallotNormalisationRule
  case object TicksForbidden extends BallotNormalisationRule
  case object CrossesForbidden extends BallotNormalisationRule
  case object CountingErrorsForbidden extends BallotNormalisationRule

}
