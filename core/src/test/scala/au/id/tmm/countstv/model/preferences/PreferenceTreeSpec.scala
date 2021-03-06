package au.id.tmm.countstv.model.preferences

import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.values.NumPapers
import au.id.tmm.countstv.{Fruit, NormalisedBallot}
import org.scalatest.FlatSpec

class PreferenceTreeSpec extends FlatSpec {

  private val ballotWith4Preferences: NormalisedBallot[Fruit] = Vector(
    Banana,
    Pear,
    Strawberry,
    Apple,
  )

  private def preferenceTreeWith(ballots: NormalisedBallot[Fruit]*) =
    PreferenceTree.from[Fruit](Set(Banana, Pear, Strawberry, Apple), ballots.length)(ballots)

  "an empty preference tree" should "have no papers" in {
    val emptyPreferenceTree = PreferenceTree.empty[Fruit](Set(Banana, Pear, Strawberry, Apple))

    assert(emptyPreferenceTree.numPapers === NumPapers(0))
  }

  "a preference tree with a single ballot" should "have 1 paper" in {
    val preferenceTree = preferenceTreeWith(
      ballotWith4Preferences,
    )

    assert(preferenceTree.numPapers === NumPapers(1))
  }

  it should "have a child for the first preference" in {
    val preferenceTree = preferenceTreeWith(
      ballotWith4Preferences,
    )

    val childNode = preferenceTree.childFor(ballotWith4Preferences.head)

    assert(childNode.exists(_.numPapers === NumPapers(1)))
  }

  it should "have a child for the last preference" in {
    val preferenceTree = preferenceTreeWith(
      ballotWith4Preferences,
    )

    val lastChildNode = preferenceTree
      .childFor(
        ballotWith4Preferences(0),
        ballotWith4Preferences(1),
        ballotWith4Preferences(2),
        ballotWith4Preferences(3),
      )

    assert(lastChildNode.exists(_.numPapers === NumPapers(1)))
  }

  it should "have no child when preferences are exhausted" in {
    val preferenceTree = preferenceTreeWith(
      ballotWith4Preferences,
    )

    val lastChildNode = preferenceTree
      .childFor(
        ballotWith4Preferences(0),
        ballotWith4Preferences(1),
        ballotWith4Preferences(2),
        ballotWith4Preferences(3),
      )
      .get

    assert(lastChildNode.childFor(Banana).isEmpty)
  }

  "a preference tree" should "have children" in {
    val preferenceTree = preferenceTreeWith(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    assert(preferenceTree.children.map(_.associatedCandidate).toSet === Set(Apple, Banana))
  }

  it should "have a string representation" in {
    val preferenceTree = preferenceTreeWith(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    assert(preferenceTree.toString === "RootPreferenceTree(numChildren=2, numPapers=3)")
  }

  it should "reject empty ballots" in {
    intercept[IllegalArgumentException] {
      preferenceTreeWith(
        Vector(Apple, Pear, Banana, Strawberry),
        Vector(Apple, Banana, Strawberry, Pear),
        Vector(),
        Vector(Banana, Pear),
      )
    }
  }

  "a preference tree child node" should "be associated with a candidate" in {
    val preferenceTree = preferenceTreeWith(
      ballotWith4Preferences,
    )

    val childNode = preferenceTree.childFor(ballotWith4Preferences.head).get

    assert(childNode.associatedCandidate === ballotWith4Preferences.head)
  }

  it should "have a string representation" in {
    val preferenceTree = preferenceTreeWith(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    val childNode = preferenceTree.childFor(Apple, Pear, Banana).get

    assert(childNode.toString === "PreferenceTreeNode(path=[Apple, Pear], numPapers=1)")
  }

  it should "not be equal to its parent" in {
    val preferenceTree = preferenceTreeWith(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    val childNode = preferenceTree.childFor(Apple, Pear, Banana).get

    assert(childNode !== preferenceTree)
  }
}
