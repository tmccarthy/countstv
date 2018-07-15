package au.id.tmm.countstv.model.preferences

import java.io.InputStream
import java.nio.ByteBuffer
import java.security.{DigestInputStream, MessageDigest}
import java.util.zip.GZIPInputStream

import au.id.tmm.countstv.model.preferences.PreferenceTableDeserialisation.Error._
import au.id.tmm.utilities.encoding.EncodingUtils.ArrayConversions

private[model] object PreferenceTableDeserialisation {

  def decompressAndDeserialise[C <: AnyRef : Ordering](allCandidates: Set[C], rawInputStream: InputStream): Either[Error, PreferenceTable[C]] = {
    val inputStream = new GZIPInputStream(rawInputStream)
    deserialise(allCandidates, inputStream)
  }

  def deserialise[C <: AnyRef : Ordering](allCandidates: Set[C], rawInputStream: InputStream): Either[Error, PreferenceTable[C]] = {
    val digest = MessageDigest.getInstance(messageDigestAlgorithm)
    val inputStream = new EndSafeInputStream(new DigestInputStream(rawInputStream, digest))

    for {
      _ <- checkMagicWord(inputStream)
      _ <- checkVersion(inputStream)
      totalNumPapers <- inputStream.readInt()
      numCandidates <- readNumCandidates(allCandidates, inputStream)
      table <- readTable(inputStream)
      actualDigest = digest.digest().toVector
      _ <- checkDigest(inputStream, actualDigest)
      _ <- confirmStreamComplete(inputStream)
    } yield {
      val rowPaperCounts = table._1
      val preferenceArrays = table._2

      val candidateIntLookup = new Array(numCandidates).asInstanceOf[Array[C]]

      allCandidates.toList.sorted.copyToArray(candidateIntLookup)

      new PreferenceTable[C](rowPaperCounts, preferenceArrays, candidateIntLookup, totalNumPapers)
    }
  }

  private def checkMagicWord(inputStream: EndSafeInputStream): Either[Error, Unit] = {
    inputStream.readBytes(magicWord.size)
      .flatMap { deserialisedMagicWord =>
        if (deserialisedMagicWord != magicWord) {
          Left(Error.MagicWordMissing(deserialisedMagicWord, magicWord))
        } else {
          Right(Unit)
        }
      }
  }

  private def checkVersion(inputStream: EndSafeInputStream): Either[Error, Unit] = {
    inputStream.readInt().flatMap { version =>
      if (version != serialisationVerson) {
        Left(Error.UnknownVersion(version))
      } else {
        Right(Unit)
      }
    }
  }

  private def readNumCandidates[C](allCandidates: Set[C], inputStream: EndSafeInputStream): Either[Error, Int] = {
    inputStream.readInt().flatMap { numCandidates =>
      if (numCandidates != allCandidates.size) {
        Left(Error.NumCandidatesMismatch(numCandidates, expectedNumCandidates = allCandidates.size))
      } else {
        Right(numCandidates)
      }
    }
  }

  private def readTable(inputStream: EndSafeInputStream): Either[Error, (Array[Int], Array[Array[Short]])] = {
    inputStream.readInt().flatMap { tableSize =>
      val rowPaperCounts = new Array[Int](tableSize)
      val preferenceArrays = new Array[Array[Short]](tableSize)

      for (i <- 0 until tableSize) {
        for {
          numPapers <- inputStream.readInt()
          preferencesLength <- inputStream.readInt()
          preferencesArray <- inputStream.readShortArray(preferencesLength)
        } yield {
          rowPaperCounts(i) = numPapers
          preferenceArrays(i) = preferencesArray
        }

      }

      Right((rowPaperCounts, preferenceArrays))
    }
  }

  private def checkDigest(inputStream: EndSafeInputStream, actualDigest: Vector[Byte]): Either[Error, Unit] = {
    inputStream.readBytes(64).map(_.toVector).flatMap { expectedDigest =>
      if (expectedDigest != actualDigest) {
        Left(DigestMismatch(actualDigest, expectedDigest, messageDigestAlgorithm))
      } else {
        Right(Unit)
      }
    }
  }

  private def confirmStreamComplete(inputStream: EndSafeInputStream): Either[UnexpectedContent, Unit] = {
    inputStream.readBytes(1).fold(_ => Right(Unit), _ => Left(UnexpectedContent()))
  }

  private final class EndSafeInputStream(inputStream: InputStream) {
    def readShortArray(length: Int): Either[PrematureStreamEnd, Array[Short]] = {
      val numBytesToRead = length * java.lang.Short.BYTES

      val bytes = new Array[Byte](numBytesToRead)

      val numBytesRead = inputStream.read(bytes)

      if (numBytesRead != numBytesToRead) return Left(PrematureStreamEnd())

      val shorts = new Array[Short](length)

      ByteBuffer.wrap(bytes).asShortBuffer().get(shorts)

      Right(shorts)
    }

    def readIntArray(length: Int): Either[PrematureStreamEnd, Array[Int]] = {
      val numBytesToRead = length * Integer.BYTES

      val bytes = new Array[Byte](numBytesToRead)

      val numBytesRead = inputStream.read(bytes)

      if (numBytesRead != numBytesToRead) return Left(PrematureStreamEnd())

      val ints = new Array[Int](length)

      ByteBuffer.wrap(bytes).asIntBuffer().get(ints)

      Right(ints)
    }

    @inline def readInts(length: Int): Either[PrematureStreamEnd, Vector[Int]] = {
      readIntArray(length).map(_.toVector)
    }

    @inline def readInt(): Either[PrematureStreamEnd, Int] = {
      readInts(1).map(_.head)
    }

    def readBytes(length: Int): Either[PrematureStreamEnd, Vector[Byte]] = {
      val array = new Array[Byte](length)
      val numBytesRead = inputStream.read(array)
      if (numBytesRead != length) {
        Left(PrematureStreamEnd())
      } else {
        Right(array.toVector)
      }
    }
  }

  sealed trait Error extends Exception {
    def message: String
    final override def getMessage: String = message
  }

  object Error {
    private def render(bytes: Vector[Byte]): String = bytes.toArray.toHex

    case class MagicWordMissing(actualMagicWord: Vector[Byte], expectedMagicWord: Vector[Byte]) extends Error {
      override def message: String = s"The magic word was missing from the start of the preference tree stream. Expected ${render(expectedMagicWord)}, found ${render(actualMagicWord)}"
    }

    case class UnknownVersion(unrecognisedVersion: Int) extends Error {
      override def message: String = s"Could not deserialise preference table serialisation version $unrecognisedVersion"
    }

    case class NumCandidatesMismatch(numCandidates: Int, expectedNumCandidates: Int) extends Error {
      override def message: String = s"The preference table contains $numCandidates, but $expectedNumCandidates " +
        s"candidates were expected"
    }

    case class DigestMismatch(actualDigest: Vector[Byte], expectedDigest: Vector[Byte], algorithm: String) extends Error {
      override def message: String = s"$messageDigestAlgorithm Integrity check failed. Expected ${render(expectedDigest)}, " +
        s"found ${render(actualDigest)}"
    }

    case class PrematureStreamEnd() extends Error {
      override def message: String = "Encountered an unexpected end of stream"
    }

    case class UnexpectedContent() extends Error {
      override def message: String = "Encountered unexpected content at end of stream"
    }
  }

}
