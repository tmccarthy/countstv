package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.ProbabilityMeasure
import au.id.tmm.utilities.collection.DupelessSeq

object TieSensitiveSorting {

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
