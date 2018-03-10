package au.id.tmm

import au.id.tmm.countstv.model.PaperBundle

import scala.collection.parallel.immutable.ParSet

package object countstv {

  type NormalisedBallot[C] = IndexedSeq[C]

  type PaperBundles[C] = ParSet[PaperBundle[C]]

}
