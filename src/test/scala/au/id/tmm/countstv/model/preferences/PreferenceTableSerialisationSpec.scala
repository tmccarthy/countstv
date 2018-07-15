package au.id.tmm.countstv.model.preferences

import java.io.InputStream

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.utilities.encoding.EncodingUtils.StringConversions
import au.id.tmm.utilities.testing.ImprovedFlatSpec
import org.scalatest.Assertion

import scala.collection.JavaConverters._

class PreferenceTableSerialisationSpec extends ImprovedFlatSpec {

  private val candidates: Set[Fruit] = Set(Apple, Banana, Strawberry, Pear)

  private def serialiseAndDeserialise(preferenceTable: PreferenceTable[Fruit]): PreferenceTable[Fruit] = {
    val inputStream = PreferenceTableSerialisation.serialise(preferenceTable)

    PreferenceTableDeserialisation.deserialise(candidates)(inputStream) match {
      case Right(preferenceTable) => preferenceTable
      case Left(error) => fail(error)
    }
  }

  private def failToDeserialise(
                                 preferenceTable: PreferenceTable[Fruit],
                                 candidates: Set[Fruit] = candidates,
                               )(
                                 modifyInts: Vector[Int] => Vector[Int],
                               ): PreferenceTableDeserialisation.Error = {
    val inputStream = PreferenceTableSerialisation.serialise(preferenceTable)

    val ints = Stream.continually(inputStream.read()).takeWhile(_ != -1).toVector

    val modifiedInts = modifyInts(ints).iterator

    val modifiedInputStream: InputStream = () => if (modifiedInts.hasNext) modifiedInts.next() else END_OF_STREAM

    PreferenceTableDeserialisation.deserialise(candidates)(modifiedInputStream) match {
      case Right(_) => fail("Expected error")
      case Left(e) => e
    }
  }

  private def assertEquals(left: PreferenceTable[Fruit], right: PreferenceTable[Fruit]): Assertion = {
    assert(left equalTo right)
  }

  private val emptyPreferenceTable = PreferenceTableConstruction.from(
    Iterable.empty[java.util.Collection[Fruit]].asJava,
    0,
    candidates.asJava,
    Fruit.ordering,
  )

  private val testPreferenceTable = {
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

    PreferenceTableConstruction.from(
      ballots.map(_.asJavaCollection).asJava,
      ballots.size,
      candidates.asJava,
      Fruit.ordering,
    )
  }

  "a preference table" can "be serialised and deserialised if it is empty" in {
    val deserialisedTable = serialiseAndDeserialise(emptyPreferenceTable)

    assertEquals(emptyPreferenceTable, deserialisedTable)
  }

  it can "be serialised if it has a non-empty table" in {
    val deserialisedTable = serialiseAndDeserialise(testPreferenceTable)

    assertEquals(testPreferenceTable, deserialisedTable)
  }

  "a sequence of bytes"  can "not be deserialised if the magic number is incorrect" in {
    val error = failToDeserialise(testPreferenceTable) { ints =>
      ints.updated(0, 0x0000)
    }

    assert(error === PreferenceTableDeserialisation.Error.MagicWordMissing())
    assert(error.getMessage === "The magic word was missing from the start of the preference tree stream")
  }

  it can "not be deserialised if the version is 2" in {
    val error = failToDeserialise(testPreferenceTable) { ints =>
      ints.updated(2, Integer.MAX_VALUE)
    }

    assert(error === PreferenceTableDeserialisation.Error.UnknownVersion(Integer.MAX_VALUE))
    assert(error.getMessage === s"Could not deserialise preference table serialisation version ${Integer.MAX_VALUE}")
  }

  it can "not be deserialised if there is a mismatch in the number of candidates" in {
    val error = failToDeserialise(testPreferenceTable, candidates + Watermelon)(modifyInts = identity)

    assert(error === PreferenceTableDeserialisation.Error.NumCandidatesMismatch(numCandidates = 4, expectedNumCandidates = 5))
    assert(error.getMessage === s"The preference table contains 4, but 5 candidates were expected")
  }

  it can "not be deserialised if the digest doesn't match" in {
    val error = failToDeserialise(testPreferenceTable) { ints =>
      ints.updated(ints.length - 74, 5)
    }

    val expectedDigest = "8f10c393dc38051329a66843d9ea615bbcaf0238894e3c79e012f73c20cf6be486610f70f9263fffeef411efe036393974c13e0a603bd7ead8229b025d33b242"
    val actualDigest = "64d6c85be6054182bb86437c44ac42acccca0ecc5ef6466cd6e0f789ba992dcdf19e985854fac29271877e5a8e316f854182e1505ef18cf847f572facf948d70"

    assert(error ===
      PreferenceTableDeserialisation.Error.DigestMismatch(
        actualDigest = actualDigest.fromHex.toVector,
        expectedDigest = expectedDigest.fromHex.toVector,
        algorithm = "SHA-512",
      )
    )
    assert(error.getMessage === s"SHA-512 Integrity check failed. Expected $expectedDigest, found $actualDigest")
  }

  it can "not be deserialised if it ends prematurely" in {
    val error = failToDeserialise(testPreferenceTable) { ints =>
      ints.take(40)
    }

    assert(error === PreferenceTableDeserialisation.Error.PrematureStreamEnd())
    assert(error.getMessage === "Encountered an unexpected end of stream")
  }

  it can "not be deserialised if it contains unexpected bytes at the end" in {
    val error = failToDeserialise(testPreferenceTable) { ints =>
      ints ++ Vector(42)
    }

    assert(error === PreferenceTableDeserialisation.Error.UnexpectedContent())
    assert(error.getMessage === "Encountered unexpected content at end of stream")
  }

}
