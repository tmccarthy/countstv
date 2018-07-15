package au.id.tmm.countstv.model.preferences

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.utilities.testing.ImprovedFlatSpec
import org.scalatest.Assertion

import scala.collection.JavaConverters._

class PreferenceTableSerialisationSpec extends ImprovedFlatSpec {

  private val candidates: Set[Fruit] = Set(Apple, Banana, Strawberry, Pear)

  private def serialiseAndDeserialise(preferenceTable: PreferenceTable[Fruit]): PreferenceTable[Fruit] = {
    val inputStream = PreferenceTableSerialisation.serialise(preferenceTable)

    PreferenceTableSerialisation.deserialise(candidates)(inputStream) match {
      case Right(preferenceTable) => preferenceTable
      case Left(error) => fail(error)
    }
  }

  private def assertEquals(left: PreferenceTable[Fruit], right: PreferenceTable[Fruit]): Assertion = {
    assert(left equalTo right)
  }

  "a preference table" can "be serialised and deserialised if it is empty" in {
    val preferenceTable = PreferenceTableConstruction.from(
      Iterable.empty[java.util.Collection[Fruit]].asJava,
      0,
      candidates.asJava,
      Fruit.ordering,
    )

    val deserialisedTable = serialiseAndDeserialise(preferenceTable)

    assertEquals(preferenceTable, deserialisedTable)
  }

  it can "be serialised if it has a non-empty table" in {
    val ballots: List[List[Fruit]] = List(
      List(Banana, Strawberry, Apple, Pear),
      List(Banana, Strawberry, Apple, Pear),
      List(Banana, Pear, Apple, Strawberry),
      List(Strawberry, Apple, Banana, Pear),
      List(Apple, Banana, Strawberry, Pear),
      List(Strawberry, Pear, Banana, Apple),
      List(Apple, Strawberry, Pear, Banana),
      List(Pear, Banana, Strawberry, Apple),
      List(Banana, Pear, Strawberry, Apple),
      List(Strawberry, Banana, Pear, Apple),
      List(Apple, Pear, Banana, Strawberry),
      List(Strawberry, Apple, Pear, Banana),
      List(Banana, Apple, Pear, Strawberry),
      List(Pear, Banana, Strawberry, Apple),
      List(Banana, Strawberry, Apple, Pear),
      List(Banana, Pear, Strawberry, Apple),
      List(Apple, Strawberry, Pear, Banana),
      List(Strawberry, Apple, Banana, Pear),
      List(Strawberry, Pear, Apple, Banana),
      List(Banana, Pear, Strawberry, Apple),
    )

    val preferenceTable = PreferenceTableConstruction.from(
      ballots.map(_.asJavaCollection).asJava,
      ballots.size,
      candidates.asJava,
      Fruit.ordering,
    )

    val deserialisedTable = serialiseAndDeserialise(preferenceTable)

    assertEquals(preferenceTable, deserialisedTable)
  }

}
