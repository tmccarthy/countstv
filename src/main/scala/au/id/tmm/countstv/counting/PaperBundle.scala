package au.id.tmm.countstv.counting

import au.id.tmm.countstv.model.PreferenceTree

private[counting] final case class PaperBundle[C](
                                                   transferValue: Double,
                                                   preferenceTreeNode: PreferenceTree[C],
                                                 )
