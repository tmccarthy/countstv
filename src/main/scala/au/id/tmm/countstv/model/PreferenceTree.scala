package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.PreferenceTree.PreferenceTreeNode
import au.id.tmm.countstv.{CandidateIndex, NormalisedBallot}

import scala.annotation.tailrec
import scala.collection.mutable

sealed class PreferenceTree[C] (val parent: Option[PreferenceTree[C]] = None) {

  private var internalNumPapers: Long = 0

  def numPapers: Long = internalNumPapers

  private val internalChildren: mutable.Map[C, PreferenceTreeNode[C]] = mutable.Map()

  def children: scala.collection.Map[C, PreferenceTreeNode[C]] = internalChildren

  private def getOrCreateChildFor(candidate: C): PreferenceTreeNode[C] =
    internalChildren.getOrElseUpdate(
      candidate,
      new PreferenceTreeNode[C](parent = Some(this), associatedCandidate = candidate),
    )

  def childFor(candidate: C): Option[PreferenceTreeNode[C]] = internalChildren.get(candidate)

  final def childFor(firstCandidate: C, subsequentCandidates: C*): Option[PreferenceTreeNode[C]] = {
    var currentChild = childFor(firstCandidate)

    for (candidate <- subsequentCandidates) {
      currentChild = currentChild.flatMap(_.childFor(candidate))
    }

    currentChild
  }

  def path: List[PreferenceTree[C]] = buildPath(previousPath = Nil)

  @tailrec
  private def buildPath(previousPath: List[PreferenceTree[C]]): List[PreferenceTree[C]] = {
    parent match {
      case Some(p) => p.buildPath(previousPath = List(this) ++ previousPath)
      case None => List(this) ++ previousPath
    }
  }

  override def toString: String = s"${getClass.getSimpleName}(numChildren=${children.size}, numPapers=$numPapers)"
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

  final class PreferenceTreeNode[C](parent: Some[PreferenceTree[C]], val associatedCandidate: C)
    extends PreferenceTree[C](parent) {

    override def toString: String = {
      val candidatePath = path
        .collect {
          case node: PreferenceTreeNode[C] => node.associatedCandidate.toString
        }
        .mkString(", ")

      s"${getClass.getSimpleName}(path=[$candidatePath], numPapers=$numPapers)"
    }
  }

}