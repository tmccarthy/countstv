package au.id.tmm.countstv.normalisation

import au.id.tmm.countstv.normalisation.BallotNormalisationRule.MinimumPreferences

object BallotNormalisation {

  sealed trait Result[C]

  object Result {

    final case class Formal[C](
                                normalisedBallot: Vector[C],
                              ) extends Result[C]

    final case class Saved[C](
                               normalisedBallot: Vector[C],
                               rulesViolated: Set[BallotNormalisationRule],
                             ) extends Result[C]

    final case class Informal[C](
                                  normalisedBallot: Vector[C],
                                  optionalRulesViolated: Set[BallotNormalisationRule],
                                  mandatoryRulesViolated: Set[BallotNormalisationRule],
                                ) extends Result[C]

  }

  def normalise[C](
                    mandatoryRules: BallotNormalisationRules,
                    optionalRules: BallotNormalisationRules,
                  )(ballot: UnNormalisedBallot[C]): Result[C] = {

    val potentialViolationsBuilder = Set.newBuilder[BallotNormalisationRule]

    def orderAccordingToPreferences(preferences: Map[C, Preference]): Vector[Set[C]] = {
      val returnedVector: scala.collection.mutable.Buffer[Set[C]] = Vector.fill(preferences.size)(Set.empty[C]).toBuffer

      @inline def isWithinValidPreferencesRange(prefAsNumber: Int) = prefAsNumber <= preferences.size
      @inline def indexForPreference(prefAsNumber: Int) = prefAsNumber - 1

      for ((x, preference) <- preferences) {
        val preferenceAsNumber = preferenceToNumber(preference)

        if (isWithinValidPreferencesRange(preferenceAsNumber)) {
          val index = indexForPreference(preferenceAsNumber)
          returnedVector.update(index, returnedVector(index) + x)
        }
      }

      returnedVector.toVector
    }

    def preferenceToNumber(preference: Preference): Int = {
      preference match {
        case Preference.Numbered(number) => number
        case Preference.Tick => {
          potentialViolationsBuilder += BallotNormalisationRule.TicksForbidden
          1
        }
        case Preference.Cross => {
          potentialViolationsBuilder += BallotNormalisationRule.CrossesForbidden
          1
        }
      }
    }

    def truncateAtCountError(rowsInPreferenceOrder: Vector[Set[C]]): Vector[C] = {
      // As long as we have only one row with each preference, we haven't encountered a count error
      rowsInPreferenceOrder
        .toStream
        .takeWhile(_.size == 1)
        .map(_.head)
        .toVector
    }

    val rowsInPreferenceOrder = orderAccordingToPreferences(ballot)

    val formalPreferences = truncateAtCountError(rowsInPreferenceOrder)

    if (formalPreferences.size < rowsInPreferenceOrder.size) {
      potentialViolationsBuilder += BallotNormalisationRule.CountingErrorsForbidden
    }

    val potentialViolations = potentialViolationsBuilder.result()

    val mandatoryRulesViolated = (potentialViolations intersect mandatoryRules.rules) ++ mandatoryRules.activeMinimumPreferencesRule.filter { case MinimumPreferences(numPreferences) => formalPreferences.size < numPreferences }
    val optionalRulesViolated = (potentialViolations intersect optionalRules.rules) ++ optionalRules.activeMinimumPreferencesRule.filter { case MinimumPreferences(numPreferences) => formalPreferences.size < numPreferences }

    if (mandatoryRulesViolated.nonEmpty) {
      Result.Informal(formalPreferences, optionalRulesViolated, mandatoryRulesViolated)
    } else if (optionalRulesViolated.nonEmpty) {
      Result.Saved(formalPreferences, optionalRulesViolated)
    } else {
      Result.Formal(formalPreferences)
    }
  }

}
