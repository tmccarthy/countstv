package au.id.tmm.countstv.counting

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.model.PreferenceTree
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class PaperBundleSpec extends ImprovedFlatSpec {

  "a paper bundle" should "have a transfer value" in {
    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = PreferenceTree.empty,
    )

    assert(paperBundle.transferValue === 0.5d)
  }

  it should "refer to a PreferenceTree" in {
    val preferenceTree: PreferenceTree[Fruit] = PreferenceTree.empty

    val paperBundle = PaperBundle[Fruit](
      transferValue = 0.5d,
      preferenceTreeNode = preferenceTree,
    )

    assert(paperBundle.preferenceTreeNode eq preferenceTree)
  }

}
