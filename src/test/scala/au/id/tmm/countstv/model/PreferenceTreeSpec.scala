package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit._
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class PreferenceTreeSpec extends ImprovedFlatSpec {

  import au.id.tmm.countstv.BallotFixtures._

  "an empty preference tree" should "have no papers" in {
    val emptyPreferenceTree = PreferenceTree.empty

    assert(emptyPreferenceTree.numPapers === 0)
  }

  "a preference tree with a single ballot" should "have 1 paper" in {
    val preferenceTree = PreferenceTree.from(List(
      ballotWith4Preferences
    ))

    assert(preferenceTree.numPapers === 1)
  }

  it should "have a child for the first preference" in {
    val preferenceTree = PreferenceTree.from(List(
      ballotWith4Preferences
    ))

    val childNode = preferenceTree.childFor(ballotWith4Preferences.head)

    assert(childNode.exists(_.numPapers === 1))
  }

  it should "have a child for the last preference" in {
    val preferenceTree = PreferenceTree.from(List(
      ballotWith4Preferences
    ))

    val lastChildNode = preferenceTree
        .childFor(
          ballotWith4Preferences(0),
          ballotWith4Preferences(1),
          ballotWith4Preferences(2),
          ballotWith4Preferences(3),
        )

    assert(lastChildNode.exists(_.numPapers === 1))
  }

  it should "have no child when preferences are exhausted" in {
    val preferenceTree = PreferenceTree.from(List(
      ballotWith4Preferences
    ))

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

  it should "have children" in {
    val preferenceTree = PreferenceTree.from(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    assert(preferenceTree.children.keySet === Set(Apple, Banana))
  }

  it should "have no parent" in {
    val preferenceTree = PreferenceTree.from(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    assert(preferenceTree.parent === None)
  }

  it should "have a string representation" in {
    val preferenceTree = PreferenceTree.from(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    assert(preferenceTree.toString === "PreferenceTree(numChildren=2, numPapers=3)")
  }

  "a preference tree child node" should "be associated with a candidate" in {
    val preferenceTree = PreferenceTree.from(List(
      ballotWith4Preferences
    ))

    val childNode = preferenceTree.childFor(ballotWith4Preferences.head).get

    assert(childNode.associatedCandidate === ballotWith4Preferences.head)
  }

  it should "have a reference to its parent" in {
    val preferenceTree = PreferenceTree.from(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    assert(preferenceTree.childFor(Apple).get.parent === Some(preferenceTree))
  }

  it should "have a path" in {
    val preferenceTree = PreferenceTree.from(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    val expectedPath = List(
      preferenceTree,
      preferenceTree.childFor(Apple).get,
      preferenceTree.childFor(Apple, Pear).get,
      preferenceTree.childFor(Apple, Pear, Banana).get,
    )

    assert(preferenceTree.childFor(Apple, Pear, Banana).get.path === expectedPath)
  }

  it should "have a string representation" in {
    val preferenceTree = PreferenceTree.from(
      Vector(Apple, Pear, Banana, Strawberry),
      Vector(Apple, Banana, Strawberry, Pear),
      Vector(Banana, Pear),
    )

    val childNode = preferenceTree.childFor(Apple, Pear, Banana).get

    assert(childNode.toString === "PreferenceTreeNode(path=[Apple, Pear, Banana], numPapers=1)")
  }
}
