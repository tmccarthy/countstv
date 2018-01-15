package au.id.tmm.countstv

import au.id.tmm.countstv.Fruit.{Apple, Banana, Pear, Strawberry}

object BallotFixtures {

  val ballotWith4Preferences: NormalisedBallot[Fruit] = Vector(
    Banana,
    Pear,
    Strawberry,
    Apple,
  )

}
