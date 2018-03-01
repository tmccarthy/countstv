package au.id.tmm

import au.id.tmm.countstv.model.PaperBundle

package object countstv {

  type NormalisedBallot[C] = IndexedSeq[C]

  type PaperBundles[C] = Set[PaperBundle[C]]

}
