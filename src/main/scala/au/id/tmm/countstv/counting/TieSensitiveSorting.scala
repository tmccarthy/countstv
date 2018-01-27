package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.ProbabilityMeasure
import au.id.tmm.utilities.collection.DupelessSeq

import scala.collection.mutable

object TieSensitiveSorting {

  def minBy[A, B](iterable: Iterable[A])
                 (f: A => B)
                 (implicit ordering: Ordering[B]): Option[ProbabilityMeasure[A]] = {
    if (iterable.isEmpty) {
      return None
    }

    val minimums: mutable.Set[A] = mutable.Set()
    var previousMin: Option[B] = None

    for (elem <- iterable) {

      val score = f(elem)

      if (previousMin.isEmpty) {
        previousMin = Some(score)
        minimums += elem

      } else if (ordering.equiv(score, previousMin.get)) {
        minimums += elem

      } else if (ordering.lt(score, previousMin.get)) {
        previousMin = Some(score)
        minimums.clear()
        minimums += elem

      }

    }

    Some(ProbabilityMeasure.allElementsEvenly(minimums))
  }

  def sortBy[A, B](iterable: Iterable[A])
                  (f: A => B)
                  (implicit ordering: Ordering[B]): ProbabilityMeasure[DupelessSeq[A]] = {

    iterable
      .toStream
      .groupBy(f)
      .toSeq
      .sortBy { case (sortKey, elements) =>
        sortKey
      }
      .map { case (sortKey, elements) =>
        ProbabilityMeasure.allElementsEvenly(elements.permutations.toList)
      }
      .foldLeft(ProbabilityMeasure.always(DupelessSeq.empty[A])) { case (acc, nextPMeasure) =>
        acc.flatMap { previousElements =>
          nextPMeasure.map { nextPossiblePermutation =>
            previousElements ++ nextPossiblePermutation
          }
        }
      }

  }

}
