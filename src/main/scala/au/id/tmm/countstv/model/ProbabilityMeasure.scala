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

  def flatMap[U](f: A => ProbabilityMeasure[U]): ProbabilityMeasure[U] = {
    val newAsMap = for {
      (possibility, branchProbability) <- asMap
      (newPossibility, subBranchProbability) <- f(possibility).asMap
    } yield newPossibility -> branchProbability * subBranchProbability

    ProbabilityMeasure(newAsMap)
  }

}

object ProbabilityMeasure {

  def always[A](possibility: A): ProbabilityMeasure[A] = evenly(possibility)

  def evenly[A](possibilities: A*): ProbabilityMeasure[A] = allElementsEvenly(possibilities)

  def allElementsEvenly[A](possibilities: Traversable[A]): ProbabilityMeasure[A] = {
    val probability = Rational(1, possibilities.size)

    val asMap = possibilities
      .map(p => p -> probability)
      .toMap

    ProbabilityMeasure(
      asMap
    )
  }

  def apply[A](branches: (A, Rational)*): ProbabilityMeasure[A] = ProbabilityMeasure(branches.toMap)
}
