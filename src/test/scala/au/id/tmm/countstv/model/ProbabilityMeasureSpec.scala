package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit
import au.id.tmm.utilities.testing.ImprovedFlatSpec
import spire.math.Rational

class ProbabilityMeasureSpec extends ImprovedFlatSpec {

  "a probability measure" should "assign a probability to each possibility" in {
    val pMeasure = ProbabilityMeasure(
      Fruit.Apple -> Rational(1, 3),
      Fruit.Banana -> Rational(1, 3),
      Fruit.Pear -> Rational(1, 6),
      Fruit.Strawberry -> Rational(1, 6),
    )

    assert(pMeasure.chanceOf(Fruit.Apple) === Rational(1, 3))
  }

  it must "sum to 1" in {
    intercept[IllegalArgumentException] {
      ProbabilityMeasure(
        Fruit.Apple -> Rational(2, 3),
        Fruit.Banana -> Rational(2, 3),
      )
    }
  }

  it can "apply an operation on the possibility" in {
    val pMeasure = ProbabilityMeasure(
      Fruit.Apple -> Rational(1, 3),
      Fruit.Banana -> Rational(2, 3),
    )

    val f: Fruit => Char = _.toString.charAt(0)

    val expectedResult = ProbabilityMeasure(
      'A' -> Rational(1, 3),
      'B' -> Rational(2, 3),
    )

    val actualResult = pMeasure.map(f)

    assert(actualResult === expectedResult)
  }

  "a possibility" must "have a positive probability" in {
    intercept[IllegalArgumentException] {
      ProbabilityMeasure(
        Fruit.Apple -> Rational(-1, 3),
        Fruit.Banana -> Rational(2, 3),
        Fruit.Strawberry -> Rational(2, 3),
      )
    }
  }

  it must "have a probability equal to or less than 1" in {
    intercept[IllegalArgumentException] {
      ProbabilityMeasure(
        Fruit.Apple -> Rational(2),
      )
    }
  }

  it can "not be duplicated" in {
    intercept[IllegalArgumentException] {
      ProbabilityMeasure(
        Fruit.Apple -> Rational(2, 3),
        Fruit.Apple -> Rational(1, 3),
      )
    }
  }

  "an unlisted possibility" should "have a possibility of zero" in {
    val pMeasure = ProbabilityMeasure(
      Fruit.Apple -> Rational(1, 3),
      Fruit.Banana -> Rational(2, 3),
    )

    assert(pMeasure.chanceOf(Fruit.Pear) === Rational.zero)
  }
}
