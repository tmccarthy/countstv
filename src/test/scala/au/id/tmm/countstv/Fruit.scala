package au.id.tmm.countstv

import au.id.tmm.utilities.collection.BiMap

sealed trait Fruit

object Fruit {
  case object Apple extends Fruit
  case object Banana extends Fruit
  case object Pear extends Fruit
  case object Strawberry extends Fruit

  val ballotOrder: BiMap[Fruit, CandidateIndex] = BiMap(
    Apple -> 1,
    Banana -> 2,
    Pear -> 3,
    Strawberry -> 4,
  )
}