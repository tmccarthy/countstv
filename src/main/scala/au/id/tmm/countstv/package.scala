package au.id.tmm

package object countstv {

  type CandidateIndex = Int
  type NormalisedBallot[C] = IndexedSeq[C]

  /**
    * Zero indexed ordinal
    */
  type Ordinal = Int

  /**
    * 1 indexed
    */
  type Count = Int

}
