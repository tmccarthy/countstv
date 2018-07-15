package au.id.tmm.countstv.model

import java.io.{ByteArrayInputStream, InputStream}
import java.security.{DigestInputStream, MessageDigest}

import au.id.tmm.countstv.model.PreferenceTableSerialisation.Error.{DigestMismatch, PrematureStreamEnd, UnexpectedContent}
import au.id.tmm.utilities.encoding.EncodingUtils.ArrayConversions

import scala.annotation.tailrec

private[model] object PreferenceTableSerialisation {

  private val END_OF_STREAM: Int = -1
  private val magicWord: Vector[Int] = Vector(0xADE1, 0xA1DE)
  private val serialisationVerson: Int = 1
  private val messageDigestAlgorithm = "SHA-512"

  def serialise[C](preferenceTable: PreferenceTable[C]): InputStream = {

    val streamBody: Stream[Int] = magicWord.toStream ++
      Stream(serialisationVerson) ++
      Stream(preferenceTable.getTotalNumPapers) ++
      Stream(preferenceTable.getCandidateLookup.length) ++
      Stream(preferenceTable.getTable.length) ++
      preferenceTable.getTable.toStream.flatMap { row =>
        row.length +: row.toStream
      }

    val bodyInputStream: InputStream = new InputStream {
      private val iterator = streamBody.iterator

      override def read(): Int = if (iterator.hasNext) iterator.next() else END_OF_STREAM
    }

    val combinedInputStream: InputStream = new InputStream {
      private val digest = MessageDigest.getInstance(messageDigestAlgorithm)
      private val wrappedBodyInputStream = new DigestInputStream(bodyInputStream, digest)

      private var digestStream: Option[InputStream] = None

      @tailrec
      override def read(): Int = {
        digestStream match {
          case Some(digestStream) => digestStream.read()
          case None => {
            val bodyInt = wrappedBodyInputStream.read()

            if (bodyInt == END_OF_STREAM) {
              digestStream = Some(new ByteArrayInputStream(digest.digest()))

              this.read()
            } else {
              bodyInt
            }
          }
        }
      }
    }

    combinedInputStream

  }

  def deserialise[C <: Object : Ordering](allCandidates: Set[C])(rawInputStream: InputStream): Either[Error, PreferenceTable[C]] = {
    val digest = MessageDigest.getInstance(messageDigestAlgorithm)
    val inputStream = new EndSafeInputStream(new DigestInputStream(rawInputStream, digest))

    for {
      _ <- checkMagicWord(inputStream)
      _ <- checkVersion(inputStream)
      totalNumPapers <- inputStream.read()
      numCandidates <- readNumCandidates(allCandidates, inputStream)
      table <- readTable(inputStream)
      actualDigest = digest.digest().toVector
      _ <- checkDigest(inputStream, actualDigest)
      _ <- confirmStreamComplete(inputStream)
    } yield {
      val candidateIntLookup = new Array(numCandidates).asInstanceOf[Array[C]]

      allCandidates.toList.sorted.copyToArray(candidateIntLookup)

      new PreferenceTable[C](
        table,
        candidateIntLookup,
        totalNumPapers,
      )
    }
  }

  private def checkMagicWord(inputStream: EndSafeInputStream): Either[Error, Unit] = {
    inputStream.read(magicWord.size)
      .flatMap { deserialisedMagicWord =>
        if (deserialisedMagicWord != magicWord) {
          Left(Error.MagicWordMissing())
        } else {
          Right(Unit)
        }
      }
  }

  private def checkVersion(inputStream: EndSafeInputStream): Either[Error, Unit] = {
    inputStream.read().flatMap { version =>
      if (version != serialisationVerson) {
        Left(Error.UnknownVersion(version))
      } else {
        Right(Unit)
      }
    }
  }

  private def readNumCandidates[C](allCandidates: Set[C], inputStream: EndSafeInputStream): Either[Error, Int] = {
    inputStream.read().flatMap { numCandidates =>
      if (numCandidates != allCandidates.size) {
        Left(Error.NumCandidatesMismatch(numCandidates, expectedNumCandidates = allCandidates.size))
      } else {
        Right(numCandidates)
      }
    }
  }

  private def readTable(inputStream: EndSafeInputStream): Either[Error, Array[Array[Int]]] = {
    inputStream.read().flatMap { tableSize =>
      val table = new Array[Array[Int]](tableSize)

      for (i <- 0 until tableSize) {
        for {
          rowLength <- inputStream.read()
          row <- inputStream.readArray(rowLength)
        } yield {
          table(i) = row
        }

      }

      Right(table)
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
    inputStream.read().fold(_ => Right(Unit), _ => Left(UnexpectedContent()))
  }

  private final class EndSafeInputStream(inputStream: InputStream) {
    @inline def read(): Either[PrematureStreamEnd, Int] = {
      val value = inputStream.read()
      if (value == END_OF_STREAM) Left(PrematureStreamEnd()) else Right(value)
    }

    def readArray(length: Int): Either[PrematureStreamEnd, Array[Int]] = {
      val array = new Array[Int](length)

      for (i <- 0 until length) {
        val value = inputStream.read()
        if (value == END_OF_STREAM) return Left(PrematureStreamEnd()) else array(i) = value
      }

      Right(array)
    }

    def read(length: Int): Either[PrematureStreamEnd, Vector[Int]] = {
      readArray(length).map(_.toVector)
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
    case class MagicWordMissing() extends Error {
      override def message: String = "The magic word was missing from the start of the preference tree stream"
    }

    case class UnknownVersion(unrecognisedVersion: Int) extends Error {
      override def message: String = s"Could not deserialise preference table serialisation version $unrecognisedVersion"
    }

    case class NumCandidatesMismatch(numCandidates: Int, expectedNumCandidates: Int) extends Error {
      override def message: String = s"The preference table contains $numCandidates, but $expectedNumCandidates " +
        s"were expected"
    }

    case class DigestMismatch(actualDigest: Vector[Byte], expectedDigest: Vector[Byte], algorithm: String) extends Error {
      override def message: String = s"$messageDigestAlgorithm Integrity check failed. Expected ${render(expectedDigest)}, " +
        s"found ${render(actualDigest)}"

      private def render(digest: Vector[Byte]): String = digest.toArray.toHex
    }

    case class PrematureStreamEnd() extends Error {
      override def message: String = "Encountered an unexpected end of stream"
    }

    case class UnexpectedContent() extends Error {
      override def message: String = "Encountered unexpected content at end of stream"
    }
  }

}
