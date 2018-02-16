package au.id.tmm.countstv

sealed trait Fruit

object Fruit {
  val ALL = List(Apple, Banana, Mango, Peach, Pear, Raspberry, Strawberry, Watermelon)

  case object Apple extends Fruit
  case object Banana extends Fruit
  case object Mango extends Fruit
  case object Peach extends Fruit
  case object Pear extends Fruit
  case object Raspberry extends Fruit
  case object Strawberry extends Fruit
  case object Watermelon extends Fruit
}
