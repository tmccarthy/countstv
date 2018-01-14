package au.id.tmm.countstv.model

import au.id.tmm.countstv.model.Fruit.{Apple, Banana, Pear, Strawberry}
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class PreferenceTreeTest extends ImprovedFlatSpec {

  private val testBallot1: NormalisedBallot[Fruit] = Vector(
    Banana,
    Pear,
    Strawberry,
    Apple,
  )

  "an empty preference tree" should "have no papers" in {
    val emptyPreferenceTree = PreferenceTree.from(List.empty)

    assert(emptyPreferenceTree.numPapers === 0)
  }

  "a preference tree with a single ballot" should "have 1 paper" in {
    val preferenceTree = PreferenceTree.from(List(
      testBallot1
    ))

    assert(preferenceTree.numPapers === 1)
  }

  it should "have a child for the first preference" in {
    val preferenceTree = PreferenceTree.from(List(
      testBallot1
    ))

    val childNode = preferenceTree.childFor(testBallot1.head)

    assert(childNode.exists(_.numPapers === 1))
  }

  it should "have a child for the last preference" in {
    val preferenceTree = PreferenceTree.from(List(
      testBallot1
    ))

    val lastChildNode = preferenceTree
        .childFor(
          testBallot1(0),
          testBallot1(1),
          testBallot1(2),
          testBallot1(3),
        )

    assert(lastChildNode.exists(_.numPapers === 1))
  }

  it should "have no child when preferences are exhausted" in {
    val preferenceTree = PreferenceTree.from(List(
      testBallot1
    ))

    val lastChildNode = preferenceTree
      .childFor(
        testBallot1(0),
        testBallot1(1),
        testBallot1(2),
        testBallot1(3),
      )
      .get

    assert(lastChildNode.childFor(Banana).isEmpty)
  }

  "a preference tree child node" should "be associated with a candidate" in {
    val preferenceTree = PreferenceTree.from(List(
      testBallot1
    ))

    val childNode = preferenceTree.childFor(testBallot1.head).get

    assert(childNode.associatedCandidate === testBallot1.head)
  }
}
