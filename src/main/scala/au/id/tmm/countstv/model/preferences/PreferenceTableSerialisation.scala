package au.id.tmm.countstv.model.preferences

import java.io.{ByteArrayInputStream, InputStream}
import java.security.{DigestInputStream, MessageDigest}

import scala.annotation.tailrec

private[model] object PreferenceTableSerialisation {

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

}
