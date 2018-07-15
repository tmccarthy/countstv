package au.id.tmm.countstv.model.preferences

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.preferences.PreferenceTableDeserialisation.Error._
import au.id.tmm.utilities.encoding.EncodingUtils.StringConversions
import au.id.tmm.utilities.testing.ImprovedFlatSpec
import org.scalatest.Assertion

import scala.collection.JavaConverters._

class PreferenceTableSerialisationSpec extends ImprovedFlatSpec {

  private val candidates: Set[Fruit] = Set(Apple, Banana, Strawberry, Pear)

  private def serialiseAndDeserialise(preferenceTable: PreferenceTable[Fruit]): PreferenceTable[Fruit] = {
    val outputStream = new ByteArrayOutputStream()

    PreferenceTableSerialisation.serialise(preferenceTable, outputStream)

    val inputStream = new ByteArrayInputStream(outputStream.toByteArray)

    PreferenceTableDeserialisation.deserialise(candidates)(inputStream) match {
      case Right(preferenceTable) => preferenceTable
      case Left(error) => fail(error)
    }
  }

  private def failToDeserialise(
                                 preferenceTable: PreferenceTable[Fruit],
                                 candidates: Set[Fruit] = candidates,
                               )(
                                 modifyStream: Vector[Byte] => Vector[Byte],
                               ): PreferenceTableDeserialisation.Error = {
    val outputStream = new ByteArrayOutputStream()

    PreferenceTableSerialisation.serialise(preferenceTable, outputStream)

    val modifiedBytes = modifyStream(outputStream.toByteArray.toVector)

    val modifiedInputStream = new ByteArrayInputStream(modifiedBytes.toArray)

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

  it can "be serialised and deserialised if it has a non-empty table" in {
    val deserialisedTable = serialiseAndDeserialise(testPreferenceTable)

    assertEquals(testPreferenceTable, deserialisedTable)
  }

  "a sequence of bytes" can "not be deserialised if the magic number is incorrect" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes.updated(0, 0.toByte)
    }

    assert(error === MagicWordMissing("00E1A1DE".fromHex.toVector, "ADE1A1DE".fromHex.toVector))
    assert(error.getMessage === "The magic word was missing from the start of the preference tree stream. Expected " +
      "ade1a1de, found 00e1a1de")
  }

  it can "not be deserialised if the version is 2" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes.updated(7, 2.toByte)
    }

    assert(error === UnknownVersion(2))
    assert(error.getMessage === s"Could not deserialise preference table serialisation version 2")
  }

  it can "not be deserialised if there is a mismatch in the number of candidates" in {
    val error = failToDeserialise(testPreferenceTable, candidates + Watermelon)(modifyStream = identity)

    assert(error === NumCandidatesMismatch(numCandidates = 4, expectedNumCandidates = 5))
    assert(error.getMessage === s"The preference table contains 4, but 5 candidates were expected")
  }

  it can "not be deserialised if the digest doesn't match" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes.updated(bytes.length - 300, 5.toByte)
    }

    val expectedDigest = "7ac351875de1d99c70e536db565334d0752880d3b52ad5fe106b0ac73cf48a71fcdd74a8a47fb87e80dc447036a3cf0930d77e119f1d225888e83b9b4d9b317c"
    val actualDigest = "0efc010d47efc368c6e6bd167546c084eb29acf2c04ac992d1e4326e537c261688618bfdcf6e49e57d2202dbd1a887429856d65c0d55c51c3824817c79624562"

    assert(error ===
      DigestMismatch(
        actualDigest = actualDigest.fromHex.toVector,
        expectedDigest = expectedDigest.fromHex.toVector,
        algorithm = "SHA-512",
      )
    )
    assert(error.getMessage === s"SHA-512 Integrity check failed. Expected $expectedDigest, found $actualDigest")
  }

  it can "not be deserialised if it ends prematurely" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes.take(160)
    }

    assert(error === PrematureStreamEnd())
    assert(error.getMessage === "Encountered an unexpected end of stream")
  }

  it can "not be deserialised if it contains unexpected bytes at the end" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes ++ Vector(42.toByte)
    }

    assert(error === UnexpectedContent())
    assert(error.getMessage === "Encountered unexpected content at end of stream")
  }

}
