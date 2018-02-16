package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.ProbabilityMeasure
import au.id.tmm.utilities.testing.ImprovedFlatSpec
import spire.math.Rational

class TieSensitiveSortingSpec extends ImprovedFlatSpec {

  "the minimum of an empty set" should "be nothing" in {
    assert(TieSensitiveSorting.min(Set[Int]()) === None)
  }

  "the minimum of a list with no duplicates" should "be the minimum" in {
    val list = List(1, 2, 3)

    assert(TieSensitiveSorting.min(list) === Some(ProbabilityMeasure.always(1)))
  }

  "the minimum of a list with some duplicates" should "be an evenly distributed probability across all the minimums" in {
    val list = List(
      "apple",
      "cat",
      "dog",
      "pear",
    )

    val scoreFn: String => Int = _.length

    val expectedMin = ProbabilityMeasure(
      "cat" -> Rational(1, 2),
      "dog" -> Rational(1, 2),
    )

    val actualMin = TieSensitiveSorting.min(list)(Ordering.by(scoreFn))

    assert(actualMin === Some(expectedMin))
  }

  "a set with no tied elements" should "have only one outcome" in {
    val set = Set(4, 2, 1, 5, 3)

    val actualResult: ProbabilityMeasure[List[Int]] = TieSensitiveSorting.sort(set)

    val expectedResult = ProbabilityMeasure.always(List(1, 2, 3, 4, 5))

    assert(actualResult === expectedResult)
  }

  "a set with one tie" should "have two even outcomes" in {
    val scores = Map(
      "A" -> 1,
      "B" -> 2,
      "C" -> 2,
      "D" -> 3,
    )

    val actualResult: ProbabilityMeasure[List[String]] = TieSensitiveSorting.sort(scores.keySet)(Ordering.by(scores))

    val expectedResult = ProbabilityMeasure.evenly(
      List("A", "B", "C", "D"),
      List("A", "C", "B", "D"),
    )

    assert(actualResult === expectedResult)
  }

  "a set with a tie between 3 outcomes and a tie between 2 outcomes" should
    "have 12 countcomes appropriately distributed" in {
    val scores = Map(
      "A" -> 1,
      "B" -> 2,
      "C" -> 2,
      "D" -> 3,
      "E" -> 3,
      "F" -> 3,
      "G" -> 5,
    )

    val actualResult = TieSensitiveSorting.sort(scores.keySet)(Ordering.by(scores))

    val expectedResult = ProbabilityMeasure(
      List("A", "B", "C", "D", "E", "F", "G") -> Rational(1, 12),
      List("A", "B", "C", "F", "D", "E", "G") -> Rational(1, 12),
      List("A", "B", "C", "E", "D", "F", "G") -> Rational(1, 12),
      List("A", "B", "C", "E", "F", "D", "G") -> Rational(1, 12),
      List("A", "B", "C", "D", "F", "E", "G") -> Rational(1, 12),
      List("A", "B", "C", "F", "E", "D", "G") -> Rational(1, 12),
      List("A", "C", "B", "D", "F", "E", "G") -> Rational(1, 12),
      List("A", "C", "B", "F", "D", "E", "G") -> Rational(1, 12),
      List("A", "C", "B", "E", "F", "D", "G") -> Rational(1, 12),
      List("A", "C", "B", "E", "D", "F", "G") -> Rational(1, 12),
      List("A", "C", "B", "F", "E", "D", "G") -> Rational(1, 12),
      List("A", "C", "B", "D", "E", "F", "G") -> Rational(1, 12),
    )

    assert(actualResult === expectedResult)
  }

}
