package au.id.tmm.countstv.model.preferences

import java.io.OutputStream
import java.nio.ByteBuffer
import java.security.{DigestOutputStream, MessageDigest}
import java.util.zip.GZIPOutputStream

private[model] object PreferenceTableSerialisation {

  def serialiseAndCompress[C](preferenceTable: PreferenceTable[C], rawOutputStream: OutputStream): Unit = {
    for (outputStream <- resource.managed(new GZIPOutputStream(rawOutputStream))) {
      serialise(preferenceTable, outputStream)
    }
  }

  def serialise[C](preferenceTable: PreferenceTable[C], rawOutputStream: OutputStream): Unit = {

    val digest = MessageDigest.getInstance(messageDigestAlgorithm)
    val outputStream = new DigestOutputStream(rawOutputStream, digest)

    writeBytes(outputStream, magicWord)

    writeInts(outputStream,
      Vector() :+
      serialisationVerson :+
      preferenceTable.getTotalNumPapers :+
      preferenceTable.getCandidateLookup.length
    )

    val table = preferenceTable.getTable
    writeInt(outputStream, table.length)

    table.foreach { row =>
      writeInt(outputStream, row.length)
      writeInts(outputStream, row)
    }

    val messageDigest = digest.digest()

    outputStream.write(messageDigest)
    outputStream.flush()
  }

  private def writeInt(outputStream: OutputStream, int: Int): Unit = writeInts(outputStream, List(int))

  private def writeInts(outputStream: OutputStream, ints: Iterable[Int]): Unit = {
    val bytes = ByteBuffer.allocate(ints.size * Integer.BYTES)

    ints.foreach(bytes.putInt)

    outputStream.write(bytes.array())
  }

  private def writeBytes(outputStream: OutputStream, bytes: Iterable[Byte]): Unit = {
    outputStream.write(bytes.toArray)
  }

}
