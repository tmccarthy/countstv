package au.id.tmm.countstv.normalisation

import au.id.tmm.countstv.normalisation.BallotNormalisationRule._

final case class BallotNormalisationRules(rules: Set[BallotNormalisationRule]) {
  private[normalisation] val activeMinimumPreferencesRule: Option[MinimumPreferences] = rules
    .collect {
      case r: MinimumPreferences => r
    }
    .toList
    .sortBy(_.numPreferences)
    .headOption
  val minimumPreferences: Option[Int]  = activeMinimumPreferencesRule.map(_.numPreferences)
  val ticksForbidden: Boolean          = rules.contains(TicksForbidden)
  val crossesForbidden: Boolean        = rules.contains(CrossesForbidden)
  val countingErrorsForbidden: Boolean = rules.contains(CountingErrorsForbidden)
}

object BallotNormalisationRules {

  val laxest: BallotNormalisationRules = BallotNormalisationRules(Set.empty)

}
