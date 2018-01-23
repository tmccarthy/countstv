package au.id.tmm.countstv.model

import spire.math.Rational

final case class ProbabilityMeasure[A](asMap: Map[A, Rational]) {
  require(asMap.valuesIterator.foldLeft(Rational.zero)(_ + _) == Rational.one)
  require(asMap.valuesIterator.forall(_ >= Rational.zero))

  def chanceOf(outcome: A): Rational = asMap.getOrElse(outcome, Rational.zero)

  def map[U](f: A => U): ProbabilityMeasure[U] = {
    val newAsMap = asMap.map { case (key, probability) =>
        f(key) -> probability
    }

    ProbabilityMeasure(newAsMap)
  }

}

object ProbabilityMeasure {
  def apply[A](branches: (A, Rational)*): ProbabilityMeasure[A] = ProbabilityMeasure(branches.toMap)
}
