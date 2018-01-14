package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.PreferenceTree.PreferenceTreeNode

import scala.annotation.tailrec
import scala.collection.mutable

sealed class PreferenceTree[C] {

  private var internalNumPapers = 0

  private val children: mutable.Map[C, PreferenceTreeNode[C]] = mutable.Map()

  private def getOrCreateChildFor(candidate: C): PreferenceTreeNode[C] =
    children.getOrElseUpdate(candidate, new PreferenceTreeNode[C](candidate))

  def numPapers: Int = internalNumPapers

  def childFor(candidate: C): Option[PreferenceTreeNode[C]] = children.get(candidate)

  final def childFor(firstCandidate: C, subsequentCandidates: C*): Option[PreferenceTreeNode[C]] = {
    var currentChild = childFor(firstCandidate)

    for (candidate <- subsequentCandidates) {
      currentChild = currentChild.flatMap(_.childFor(candidate))
    }

    currentChild
  }

}

object PreferenceTree {

  def from[C](
               ballots: Iterable[NormalisedBallot[C]],
             ): PreferenceTree[C] = {
    val returnedPreferenceTree = new PreferenceTree[C]()

    for (ballot <- ballots) {
      incrementPaperCount(returnedPreferenceTree, ballot)
    }

    returnedPreferenceTree
  }

  @tailrec
  private def incrementPaperCount[C](
                                      preferenceTreeToIncrement: PreferenceTree[C],
                                      ballot: NormalisedBallot[C],
                                      incrementFromIndex: CandidateIndex = 0,
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

  final class PreferenceTreeNode[C](val associatedCandidate: C) extends PreferenceTree[C]

}