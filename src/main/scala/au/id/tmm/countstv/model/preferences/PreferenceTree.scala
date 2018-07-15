package au.id.tmm.countstv.model.preferences

import au.id.tmm.countstv.NormalisedBallot
import au.id.tmm.countstv.model.preferences.PreferenceTree.PreferenceTreeNode
import au.id.tmm.countstv.model.values.NumPapers

import scala.collection.JavaConverters
import scala.collection.JavaConverters.asJavaIterableConverter

sealed class PreferenceTree[C] (private val view: PreferenceTable.View[C]) {

  /**
    * The number of ballot papers that this node represents.
    */
  def numPapers: NumPapers = NumPapers(view.numPapers.toLong)

  /**
    * The child nodes of this node.
    */
  def children: List[PreferenceTreeNode[C]] = view.children().map(new PreferenceTreeNode(_))

  def childFor(candidate: C): Option[PreferenceTreeNode[C]] = children.find(_.associatedCandidate == candidate)

  final def childFor(firstCandidate: C, subsequentCandidates: C*): Option[PreferenceTreeNode[C]] = {
    var currentChild = childFor(firstCandidate)

    for (candidate <- subsequentCandidates) {
      currentChild = currentChild.flatMap(_.childFor(candidate))
    }

    currentChild
  }

  override def toString: String = s"${getClass.getSimpleName}(numChildren=${children.size}, $numPapers)"

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

  def empty[C: Ordering](allCandidates: Set[C]): PreferenceTree[C] = from(allCandidates, numBallotsHint = 0)(Nil)

  def from[C : Ordering](allCandidates: Set[C], numBallotsHint: Int = 100)(ballots: Iterable[NormalisedBallot[C]]): PreferenceTree[C] = {

    val ballotsAsJava = ballots
      .map { b =>
        require(b.nonEmpty)
        b
      }
      .map(JavaConverters.seqAsJavaList(_).asInstanceOf[java.util.Collection[C]])
      .asJava

    val preferenceTable = PreferenceTableConstruction.from(
      ballotsAsJava,
      numBallotsHint,
      JavaConverters.setAsJavaSet(allCandidates),
      implicitly[Ordering[C]],
    )

    new PreferenceTree[C](preferenceTable.rootView())
  }

  final class PreferenceTreeNode[C](private val view: PreferenceTable.View[C]) extends PreferenceTree[C](view) {
    def associatedCandidate: C = view.assignedCandidate().get
  }

}