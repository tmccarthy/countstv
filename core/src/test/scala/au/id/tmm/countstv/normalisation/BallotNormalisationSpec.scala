package au.id.tmm.countstv.normalisation

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.normalisation.BallotNormalisation.Result
import org.scalatest.FlatSpec

import scala.collection.immutable.ArraySeq

class BallotNormalisationSpec extends FlatSpec {

  private def UnNormalisedBallot(preferences: (Fruit, Any)*): UnNormalisedBallot[Fruit] =
    preferences
      .toMap
      .view
      .mapValues {
        case i: Int => Preference.Numbered(i)
        case "✓" => Preference.Tick
        case "x" => Preference.Cross
      }
      .toMap

  private def testRule(
    rule: BallotNormalisationRule,

    passingBallots: List[(String, (UnNormalisedBallot[Fruit], ArraySeq[Fruit]))],
    violatingBallots: List[(String, (UnNormalisedBallot[Fruit], ArraySeq[Fruit]))],
  ): Unit = {
    behavior of rule.toString

    val rules = BallotNormalisationRules(Set(rule))

    passingBallots.foreach { case (ballotDescription, (unNormalisedBallot, expectedNormalisedBallot)) =>
      it should s"allow a $ballotDescription" in {
        val normalisedBallot = BallotNormalisation.normalise(rules, rules)(unNormalisedBallot)

        assert(normalisedBallot === BallotNormalisation.Result.Formal(expectedNormalisedBallot))
      }
    }

    violatingBallots.foreach { case (ballotDescription, (unNormalisedBallot, expectedNormalisedBallot)) =>
      it should s"warn for $ballotDescription when it is an optional rule" in {
        val normalisedBallot = BallotNormalisation.normalise(BallotNormalisationRules.laxest, rules)(unNormalisedBallot)

        normalisedBallot match {
          case Result.Saved(normalisedBallot, rulesViolated) => {
            assert(normalisedBallot === expectedNormalisedBallot)
            assert(rulesViolated contains rule)
          }
          case unexpectedResult => fail(unexpectedResult.toString)
        }
      }

      it should s"fail for $ballotDescription when it is an optional rule" in {
        val normalisedBallot = BallotNormalisation.normalise(rules, BallotNormalisationRules.laxest)(unNormalisedBallot)

        normalisedBallot match {
          case Result.Informal(normalisedBallot, rulesViolated, mandatoryRulesViolated) => {
            assert(normalisedBallot === expectedNormalisedBallot)
            assert(mandatoryRulesViolated contains rule)
          }
          case unexpectedResult => fail(unexpectedResult.toString)
        }
      }
    }
  }

  testRule(
    BallotNormalisationRule.MinimumPreferences(3),
    passingBallots = List(
      "a ballot with 5 preferences" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 3, Peach -> 4, Pear -> 5) -> ArraySeq(Apple, Banana, Mango, Peach, Pear),
        ),
      "a ballot that misses a number after the third preference" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 3, Peach -> 4, Pear -> 6) -> ArraySeq(Apple, Banana, Mango, Peach),
        ),
      "a ballot that repeats a number after the third preference" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 3, Peach -> 4, Pear -> 4, Raspberry -> 5) -> ArraySeq(Apple, Banana, Mango),
        ),
    ),
    violatingBallots = List(
      "a ballot with 2 preferences" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2) -> ArraySeq(Apple, Banana),
        ),
      "a ballot that misses a number before the third preference" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 4, Peach -> 5) -> ArraySeq(Apple, Banana),
        ),
      "a ballot that repeats a number before the third preference" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 3, Peach -> 3, Pear -> 4) -> ArraySeq(Apple, Banana),
        ),
    )
  )

  testRule(
    BallotNormalisationRule.TicksForbidden,
    passingBallots = List(
      "a ballot with 5 preferences" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 3, Peach -> 4, Pear -> 5) -> ArraySeq(Apple, Banana, Mango, Peach, Pear),
        ),
    ),
    violatingBallots = List(
      "a ballot with a tick" -> (
        UnNormalisedBallot(Apple -> "✓", Banana -> 2) -> ArraySeq(Apple, Banana),
        ),
    )
  )

  testRule(
    BallotNormalisationRule.CrossesForbidden,
    passingBallots = List(
      "a ballot with 5 preferences" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 3, Peach -> 4, Pear -> 5) -> ArraySeq(Apple, Banana, Mango, Peach, Pear),
        ),
    ),
    violatingBallots = List(
      "a ballot with a tick" -> (
        UnNormalisedBallot(Apple -> "x", Banana -> 2) -> ArraySeq(Apple, Banana),
        ),
    )
  )

  testRule(
    BallotNormalisationRule.CountingErrorsForbidden,
    passingBallots = List(
      "a ballot with 5 preferences" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 3, Peach -> 4, Pear -> 5) -> ArraySeq(Apple, Banana, Mango, Peach, Pear),
        ),
    ),
    violatingBallots = List(
      "a ballot that misses a number" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 4, Peach -> 5) -> ArraySeq(Apple, Banana),
        ),
      "a ballot that repeats a number" -> (
        UnNormalisedBallot(Apple -> 1, Banana -> 2, Mango -> 3, Peach -> 3, Pear -> 4) -> ArraySeq(Apple, Banana),
        ),
    )
  )

}
