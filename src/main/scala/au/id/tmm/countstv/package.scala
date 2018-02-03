package au.id.tmm

import au.id.tmm.countstv.model.PaperBundle

import scala.collection.immutable.Bag

package object countstv {

  type NormalisedBallot[C] = IndexedSeq[C]

  type PaperBundles[C] = Bag[PaperBundle[C]]

  /**
    * Zero indexed ordinal
    */
  type Ordinal = Int // TODO make this a value class
}
