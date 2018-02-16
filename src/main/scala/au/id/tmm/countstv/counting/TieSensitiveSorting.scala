package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.ProbabilityMeasure

import scala.collection.mutable

object TieSensitiveSorting {

  def sort[A](iterable: Iterable[A])(implicit ordering: Ordering[A]): ProbabilityMeasure[List[A]] = {
    iterable
      .toStream
      .sorted(ordering)
      .foldLeft(List.empty[List[A]]) { case (acc, newElem) =>
          if (acc.nonEmpty && ordering.equiv(acc.last.head, newElem)) {
            acc.init :+ (acc.last :+ newElem)
          } else {
            acc :+ List(newElem)
          }
      }
      .map { elements =>
        ProbabilityMeasure.allElementsEvenly(elements.permutations.toList)
      }
      .foldLeft(ProbabilityMeasure.always(List.empty[A])) { case (acc, nextPMeasure) =>
        acc.flatMap { previousElements =>
          nextPMeasure.map { nextPossiblePermutation =>
            previousElements ++ nextPossiblePermutation
          }
        }
      }
  }

  def min[A](iterable: Iterable[A])(implicit ordering: Ordering[A]): Option[ProbabilityMeasure[A]] = {
    if (iterable.isEmpty) {
      return None
    }

    val minimums: mutable.Set[A] = mutable.Set()

    for (elem <- iterable) {

      if (minimums.isEmpty) {
        minimums += elem

      } else if (ordering.equiv(elem, minimums.head)) {
        minimums += elem

      } else if (ordering.lt(elem, minimums.head)) {
        minimums.clear()
        minimums += elem

      }

    }

    Some(ProbabilityMeasure.allElementsEvenly(minimums))
  }

}
