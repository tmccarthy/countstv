package au.id.tmm.countstv.model.preferences

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.preferences.PreferenceTableDeserialisation.Error._
import au.id.tmm.utilities.codec.binarycodecs._
import org.scalatest.{Assertion, FlatSpec}

import scala.jdk.CollectionConverters._

class PreferenceTableSerialisationSpec extends FlatSpec {

  private val candidates: Set[Fruit] = Set(Apple, Banana, Strawberry, Pear)

  private def serialiseAndDeserialise(preferenceTable: PreferenceTable[Fruit]): PreferenceTable[Fruit] = {
    val outputStream = new ByteArrayOutputStream()

    PreferenceTableSerialisation.serialise(preferenceTable, outputStream)

    val inputStream = new ByteArrayInputStream(outputStream.toByteArray)

    PreferenceTableDeserialisation.deserialise(candidates, inputStream) match {
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

    PreferenceTableDeserialisation.deserialise(candidates, modifiedInputStream) match {
      case Right(_) => fail("Expected error")
      case Left(e) => e
    }
  }

  private def assertEquals(left: PreferenceTable[Fruit], right: PreferenceTable[Fruit]): Assertion = {
    assert(left equalTo right)
  }

  private val emptyPreferenceTable = PreferenceTableConstruction.from(
    Iterator.empty.asInstanceOf[Iterator[java.util.Collection[Fruit]]].asJava,
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
      ballots.map(_.asJavaCollection).iterator.asJava,
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

    it can "be serialised and deserialised with compression" in {
      val outputStream = new ByteArrayOutputStream()

      PreferenceTableSerialisation.serialiseAndCompress(testPreferenceTable, outputStream)

      val inputStream = new ByteArrayInputStream(outputStream.toByteArray)

      val deserialisedPreferenceTable = PreferenceTableDeserialisation.decompressAndDeserialise(candidates, inputStream) match {
        case Right(preferenceTable) => preferenceTable
        case Left(error) => fail(error)
      }

      assertEquals(deserialisedPreferenceTable, testPreferenceTable)
    }

  "a sequence of bytes" can "not be deserialised if the magic number is incorrect" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes.updated(0, 0.toByte)
    }

    assert(error === MagicWordMissing(hex"00E1A1DE", hex"ADE1A1DE", 4))
    assert(error.getMessage === "The magic word was missing from the start of the preference tree stream. Expected " +
      "ade1a1de, found 00e1a1de at byte 4")
  }

  it can "not be deserialised if the version is 2" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes.updated(7, 2.toByte)
    }

    assert(error === UnknownVersion(2, streamPosition = 8))
    assert(error.getMessage === s"Could not deserialise preference table serialisation version 2 at byte 8")
  }

  it can "not be deserialised if there is a mismatch in the number of candidates" in {
    val error = failToDeserialise(testPreferenceTable, candidates + Watermelon)(modifyStream = identity)

    assert(error === NumCandidatesMismatch(numCandidates = 4, expectedNumCandidates = 5, streamPosition = 16))
    assert(error.getMessage === s"The preference table contains 4, but 5 candidates were expected at byte 16")
  }

  it can "not be deserialised if the digest doesn't match" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes.updated(bytes.length - 200, 5.toByte)
    }

    val expectedDigest = "5eb8709a96ee5191569879a59eea8255297c8fd7358e9ca5a57c288520d6bfc5b77c5d8d460499aeb7d884e390bafce232ad25de8155b9524596611b92eed051"
    val actualDigest = "09548af2d26deea3f2de53f715924c586b9e522f4014a9224256ef47baa1ed73cad3518da493a2768891fdf05cf0285a972e0287fd338e370b76d1899097fe11"

    assert(error ===
      DigestMismatch(
        actualDigest = actualDigest.parseHexUnsafe,
        expectedDigest = expectedDigest.parseHexUnsafe,
        algorithm = "SHA-512",
        streamPosition = 292,
      )
    )
    assert(error.getMessage === s"SHA-512 Integrity check failed. Expected $expectedDigest, found $actualDigest at byte 292")
  }

  it can "not be deserialised if it ends prematurely" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes.take(160)
    }

    assert(error === PrematureStreamEnd(streamPosition = 160))
    assert(error.getMessage === "Encountered an unexpected end of stream at byte 160")
  }

  it can "not be deserialised if it contains unexpected bytes at the end" in {
    val error = failToDeserialise(testPreferenceTable) { bytes =>
      bytes ++ Vector(42.toByte)
    }

    assert(error === UnexpectedContent(streamPosition = 293))
    assert(error.getMessage === "Encountered unexpected content at end of stream at byte 293")
  }

}
