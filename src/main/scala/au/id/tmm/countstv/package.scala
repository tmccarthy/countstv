package au.id.tmm

import au.id.tmm.countstv.model.PaperBundle

import scala.collection.immutable.Bag

package object countstv {

  type CandidateIndex = Int
  type NormalisedBallot[C] = IndexedSeq[C]

  type PaperBundles[C] = Bag[PaperBundle[C]]

  /**
    * Zero indexed ordinal
    */
  type Ordinal = Int

  /**
    * 1 indexed
    */
  type Count = Int

}
