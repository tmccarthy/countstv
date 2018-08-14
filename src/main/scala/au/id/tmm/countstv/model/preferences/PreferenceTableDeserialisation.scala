package au.id.tmm.countstv.model.preferences

import java.io.{DataInputStream, EOFException, InputStream}
import java.nio.ByteBuffer
import java.security.{DigestInputStream, MessageDigest}
import java.util.zip.GZIPInputStream

import au.id.tmm.countstv.model.preferences.PreferenceTableDeserialisation.Error._
import au.id.tmm.utilities.encoding.EncodingUtils.ArrayConversions
import com.google.common.io.CountingInputStream

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
          Left(Error.MagicWordMissing(deserialisedMagicWord, magicWord, inputStream.position()))
        } else {
          Right(Unit)
        }
      }
  }

  private def checkVersion(inputStream: EndSafeInputStream): Either[Error, Unit] = {
    inputStream.readInt().flatMap { version =>
      if (version != serialisationVerson) {
        Left(Error.UnknownVersion(version, inputStream.position()))
      } else {
        Right(Unit)
      }
    }
  }

  private def readNumCandidates[C](allCandidates: Set[C], inputStream: EndSafeInputStream): Either[Error, Int] = {
    inputStream.readInt().flatMap { numCandidates =>
      if (numCandidates != allCandidates.size) {
        Left(Error.NumCandidatesMismatch(numCandidates, expectedNumCandidates = allCandidates.size, inputStream.position()))
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
        (for {
          numPapers <- inputStream.readInt()
          preferencesLength <- inputStream.readInt()
          preferencesArray <- inputStream.readShortArray(preferencesLength)
        } yield {
          rowPaperCounts(i) = numPapers
          preferenceArrays(i) = preferencesArray
        }) match {
          case Left(error) => return Left(error)
          case _ =>
        }
      }

      Right((rowPaperCounts, preferenceArrays))
    }
  }

  private def checkDigest(inputStream: EndSafeInputStream, actualDigest: Vector[Byte]): Either[Error, Unit] = {
    inputStream.readBytes(64).map(_.toVector).flatMap { expectedDigest =>
      if (expectedDigest != actualDigest) {
        Left(DigestMismatch(actualDigest, expectedDigest, messageDigestAlgorithm, inputStream.position()))
      } else {
        Right(Unit)
      }
    }
  }

  private def confirmStreamComplete(inputStream: EndSafeInputStream): Either[UnexpectedContent, Unit] = {
    inputStream.readBytes(1).fold(_ => Right(Unit), _ => Left(UnexpectedContent(inputStream.position())))
  }

  private final class EndSafeInputStream(rawInputStream: InputStream) {
    private val countingInputStream = new CountingInputStream(rawInputStream)
    private val dataInputStream = new DataInputStream(countingInputStream)

    def position(): Long = countingInputStream.getCount

    def readShortArray(length: Int): Either[PrematureStreamEnd, Array[Short]] = {
      val numBytesToRead = length * java.lang.Short.BYTES

      val bytes = new Array[Byte](numBytesToRead)

      try {
        dataInputStream.readFully(bytes)

        val shorts = new Array[Short](length)

        ByteBuffer.wrap(bytes).asShortBuffer().get(shorts)

        Right(shorts)
      } catch {
        case _: EOFException => Left(PrematureStreamEnd(position()))
      }
    }

    @inline def readInt(): Either[PrematureStreamEnd, Int] = {
      try {
        Right(dataInputStream.readInt())
      } catch {
        case _: EOFException => Left(PrematureStreamEnd(position()))
      }
    }

    def readBytes(length: Int): Either[PrematureStreamEnd, Vector[Byte]] = {
      val array = new Array[Byte](length)

      try {
        dataInputStream.readFully(array)

        Right(array.toVector)
      } catch {
        case _: EOFException => Left(PrematureStreamEnd(position()))
      }
    }
  }

  sealed trait Error extends Exception {
    def message: String
    final override def getMessage: String = s"$message at byte $streamPosition"
    def streamPosition: Long
  }

  object Error {
    private def render(bytes: Vector[Byte]): String = bytes.toArray.toHex

    case class MagicWordMissing(actualMagicWord: Vector[Byte], expectedMagicWord: Vector[Byte], streamPosition: Long) extends Error {
      override def message: String = s"The magic word was missing from the start of the preference tree stream. Expected ${render(expectedMagicWord)}, found ${render(actualMagicWord)}"
    }

    case class UnknownVersion(unrecognisedVersion: Int, streamPosition: Long) extends Error {
      override def message: String = s"Could not deserialise preference table serialisation version $unrecognisedVersion"
    }

    case class NumCandidatesMismatch(numCandidates: Int, expectedNumCandidates: Int, streamPosition: Long) extends Error {
      override def message: String = s"The preference table contains $numCandidates, but $expectedNumCandidates " +
        s"candidates were expected"
    }

    case class DigestMismatch(actualDigest: Vector[Byte], expectedDigest: Vector[Byte], algorithm: String, streamPosition: Long) extends Error {
      override def message: String = s"$messageDigestAlgorithm Integrity check failed. Expected ${render(expectedDigest)}, " +
        s"found ${render(actualDigest)}"
    }

    case class PrematureStreamEnd(streamPosition: Long) extends Error {
      override def message: String = "Encountered an unexpected end of stream"
    }

    case class UnexpectedContent(streamPosition: Long) extends Error {
      override def message: String = "Encountered unexpected content at end of stream"
    }
  }

}
