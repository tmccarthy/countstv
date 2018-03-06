package au.id.tmm.countstv.model

import au.id.tmm.countstv.NormalisedBallot
import au.id.tmm.countstv.model.PreferenceTree.PreferenceTreeNode
import au.id.tmm.countstv.model.values.NumPapers

import scala.annotation.tailrec
import scala.collection.mutable

sealed class PreferenceTree[C] () {

  private var internalNumPapers: Long = 0

  def numPapers: NumPapers = NumPapers(internalNumPapers)

  private val internalChildren: mutable.Map[C, PreferenceTreeNode[C]] = new mutable.OpenHashMap(5)

  def children: scala.collection.Map[C, PreferenceTreeNode[C]] = internalChildren

  private def getOrCreateChildFor(candidate: C): PreferenceTreeNode[C] =
    internalChildren.getOrElseUpdate(
      candidate,
      new PreferenceTreeNode[C](associatedCandidate = candidate),
    )

  def childFor(candidate: C): Option[PreferenceTreeNode[C]] = internalChildren.get(candidate)

  final def childFor(firstCandidate: C, subsequentCandidates: C*): Option[PreferenceTreeNode[C]] = {
    var currentChild = childFor(firstCandidate)

    for (candidate <- subsequentCandidates) {
      currentChild = currentChild.flatMap(_.childFor(candidate))
    }

    currentChild
  }

  override def toString: String = s"${getClass.getSimpleName}(numChildren=${children.size}, $numPapers)"
}

object PreferenceTree {

  def empty[C]: PreferenceTree[C] = new PreferenceTree[C]()

  def from[C](ballots: NormalisedBallot[C]*): PreferenceTree[C] = from(ballots)

  def from[C](ballots: Iterable[NormalisedBallot[C]]): PreferenceTree[C] = {
    val returnedPreferenceTree = new PreferenceTree[C]()

    for (ballot <- ballots) {
      require(ballot.nonEmpty)

      incrementPaperCount(returnedPreferenceTree, ballot)
    }

    returnedPreferenceTree
  }

  @tailrec
  private def incrementPaperCount[C](
                                      preferenceTreeToIncrement: PreferenceTree[C],
                                      ballot: NormalisedBallot[C],
                                      incrementFromIndex: Int = 0,
                                    ): Unit = {
    preferenceTreeToIncrement.internalNumPapers = preferenceTreeToIncrement.internalNumPapers + 1

    if (incrementFromIndex < ballot.size) {
      val nextCandidateToIncrement = ballot(incrementFromIndex)

      val nextPreferenceTreeToIncrement = preferenceTreeToIncrement.getOrCreateChildFor(nextCandidateToIncrement)

      incrementPaperCount(
        nextPreferenceTreeToIncrement,
        ballot,
        incrementFromIndex + 1,
      )
    }
  }

  final class PreferenceTreeNode[C](val associatedCandidate: C)
    extends PreferenceTree[C] {
  }

}