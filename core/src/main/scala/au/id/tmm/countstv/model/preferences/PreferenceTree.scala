package au.id.tmm.countstv.model.preferences

import au.id.tmm.countstv.NormalisedBallot
import au.id.tmm.countstv.model.preferences.PreferenceTree.PreferenceTreeNode
import au.id.tmm.countstv.model.values.NumPapers

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._

sealed abstract class PreferenceTree[C] private (private val view: PreferenceTable.View[C]) {

  /**
    * The number of ballot papers that this node represents.
    */
  def numPapers: NumPapers = NumPapers(view.numPapers.toLong)

  /**
    * The child nodes of this node.
    */
  def children: ArraySeq[PreferenceTreeNode[C]] = view.children().map(new PreferenceTreeNode[C](_))

  def childFor(candidate: C): Option[PreferenceTreeNode[C]] = children.find(_.associatedCandidate == candidate)

  final def childFor(firstCandidate: C, subsequentCandidates: C*): Option[PreferenceTreeNode[C]] = {
    var currentChild = childFor(firstCandidate)

    for (candidate <- subsequentCandidates) {
      currentChild = currentChild.flatMap(_.childFor(candidate))
    }

    currentChild
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[PreferenceTree[_]]

  override def equals(other: Any): Boolean = other match {
    case that: PreferenceTree[_] =>
      (that canEqual this) &&
        view == that.view
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(view)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object PreferenceTree {

  def empty[C: Ordering](allCandidates: Set[C]): RootPreferenceTree[C] = from(allCandidates, numBallotsHint = 0)(Nil)

  def from[C : Ordering](
                          allCandidates: Set[C],
                          numBallotsHint: Int = 100,
                        )(
                          ballots: Iterable[NormalisedBallot[C]],
                        ): RootPreferenceTree[C] = fromIterator(allCandidates, numBallotsHint)(ballots.iterator)

  def fromIterator[C : Ordering](
                                  allCandidates: Set[C],
                                  numBallotsHint: Int = 100,
                                )(
                                  ballots: Iterator[NormalisedBallot[C]],
                                ): RootPreferenceTree[C] = {

    val ballotsAsJava = ballots
      .map { b =>
        require(b.nonEmpty)
        b
      }
      .map(ballot => ballot.asJavaCollection)
      .asJava

    val preferenceTable = PreferenceTableConstruction.from(
      ballotsAsJava,
      numBallotsHint,
      allCandidates.asJavaCollection,
      implicitly[Ordering[C]],
    )

    new RootPreferenceTree[C](preferenceTable)
  }

  final class RootPreferenceTree[C] private[preferences] (private[preferences] val preferenceTable: PreferenceTable[C])
    extends PreferenceTree[C](preferenceTable.rootView()) {

    override def toString: String = s"${getClass.getSimpleName}(numChildren=${children.size}, numPapers=${numPapers.asLong})"
  }

  final class PreferenceTreeNode[C] private[preferences] (private val view: PreferenceTable.View[C]) extends PreferenceTree[C](view) {
    def associatedCandidate: C = view.assignedCandidate().get

    override def toString: String = {
      val candidatePath = view.path().mkString(", ")

      s"${getClass.getSimpleName}(path=[$candidatePath], numPapers=${numPapers.asLong})"
    }
  }

}