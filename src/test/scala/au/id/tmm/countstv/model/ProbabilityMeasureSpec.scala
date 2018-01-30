package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.utilities.testing.ImprovedFlatSpec
import spire.math.Rational

class ProbabilityMeasureSpec extends ImprovedFlatSpec {

  "a probability measure" should "assign a probability to each possibility" in {
    val pMeasure = ProbabilityMeasure(
      Apple -> Rational(1, 3),
      Banana -> Rational(1, 3),
      Pear -> Rational(1, 6),
      Strawberry -> Rational(1, 6),
    )

    assert(pMeasure.chanceOf(Apple) === Rational(1, 3))
  }

  it must "sum to 1" in {
    intercept[IllegalArgumentException] {
      ProbabilityMeasure(
        Apple -> Rational(2, 3),
        Banana -> Rational(2, 3),
      )
    }
  }

  it can "apply an operation on the possibility" in {
    val pMeasure = ProbabilityMeasure(
      Apple -> Rational(1, 3),
      Banana -> Rational(2, 3),
    )

    val f: Fruit => Char = _.toString.charAt(0)

    val expectedResult = ProbabilityMeasure(
      'A' -> Rational(1, 3),
      'B' -> Rational(2, 3),
    )

    val actualResult = pMeasure.map(f)

    assert(actualResult === expectedResult)
  }

  it can "map a possibility to another ProbabilityMeasure" in {
    val pMeasure = ProbabilityMeasure(
      Apple -> Rational(1, 3),
      Banana -> Rational(2, 3),
    )

    val f: Fruit => ProbabilityMeasure[(Fruit, Fruit)] = {
      case Apple => ProbabilityMeasure(
        (Apple, Pear) -> Rational(3, 4),
        (Apple, Strawberry) -> Rational(1, 4),
      )
      case Banana => ProbabilityMeasure(
        (Banana, Pear) -> Rational(2, 5),
        (Banana, Strawberry) -> Rational(3, 5),
      )
    }

    val expectedResult = ProbabilityMeasure(
      (Apple, Pear) -> Rational(1, 3) * Rational(3, 4),
      (Apple, Strawberry) -> Rational(1, 3) * Rational(1, 4),
      (Banana, Pear) -> Rational(2, 3) * Rational(2, 5),
      (Banana, Strawberry) -> Rational(2, 3) * Rational(3, 5),
    )

    val actualResult = pMeasure.flatMap(f)

    assert(actualResult === expectedResult)
  }

  it can "be built from a number of evenly distributed possibilities" in {
    val actualResult = ProbabilityMeasure.evenly(Apple, Pear, Strawberry)

    val expectedResult = ProbabilityMeasure(
      Apple -> Rational(1, 3),
      Pear -> Rational(1, 3),
      Strawberry -> Rational(1, 3),
    )

    assert(actualResult === expectedResult)
  }

  "a probability measure with a single possibility" should "have a probability of 1" in {
    assert(ProbabilityMeasure.always(Apple).chanceOf(Apple) === Rational.one)
  }

  it should "have no probability of another outcome" in {
    assert(ProbabilityMeasure.always[Fruit](Apple).chanceOf(Banana) === Rational.zero)
  }

  "a possibility" must "have a positive probability" in {
    intercept[IllegalArgumentException] {
      ProbabilityMeasure(
        Apple -> Rational(-1, 3),
        Banana -> Rational(2, 3),
        Strawberry -> Rational(2, 3),
      )
    }
  }

  it must "have a probability equal to or less than 1" in {
    intercept[IllegalArgumentException] {
      ProbabilityMeasure(
        Apple -> Rational(2),
      )
    }
  }

  it can "not be duplicated" in {
    intercept[IllegalArgumentException] {
      ProbabilityMeasure(
        Apple -> Rational(2, 3),
        Apple -> Rational(1, 3),
      )
    }
  }

  "an unlisted possibility" should "have a possibility of zero" in {
    val pMeasure = ProbabilityMeasure(
      Apple -> Rational(1, 3),
      Banana -> Rational(2, 3),
    )

    assert(pMeasure.chanceOf(Pear) === Rational.zero)
  }
}
