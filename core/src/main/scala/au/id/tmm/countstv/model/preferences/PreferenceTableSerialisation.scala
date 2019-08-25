package au.id.tmm.countstv.model.preferences

import java.io.OutputStream
import java.nio.ByteBuffer
import java.security.{DigestOutputStream, MessageDigest}
import java.util.zip.GZIPOutputStream

private[model] object PreferenceTableSerialisation {

  def serialiseAndCompress[C](preferenceTable: PreferenceTable[C], rawOutputStream: OutputStream): Unit =
    try {
      val outputStream = new GZIPOutputStream(rawOutputStream)
      serialise(preferenceTable, outputStream)
    } finally {
      rawOutputStream.close()
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

    writeInt(outputStream, preferenceTable.getLength)

    for (i <- 0 until preferenceTable.getLength) {
      writeInt(outputStream, preferenceTable.getRowPaperCounts()(i))
      writeInt(outputStream, preferenceTable.getPreferenceArrays()(i).length)
      writeShorts(outputStream, preferenceTable.getPreferenceArrays()(i))
    }

    val messageDigest = digest.digest()

    outputStream.write(messageDigest)
    outputStream.flush()
  }

  private def writeInt(outputStream: OutputStream, int: Int): Unit = writeInts(outputStream, List(int))

  private def writeInts(outputStream: OutputStream, ints: Iterable[Int]): Unit = {
    val bytes = ByteBuffer.allocate(ints.size * Integer.BYTES)

    ints.foreach { int =>
      assert(int >= 0)
      bytes.putInt(int)
    }

    outputStream.write(bytes.array())
  }

  private def writeShorts(outputStream: OutputStream, shorts: Iterable[Short]): Unit = {
    val bytes = ByteBuffer.allocate(shorts.size * java.lang.Short.BYTES)

    shorts.foreach { short =>
      assert(short >= 0)
      bytes.putShort(short)
    }

    outputStream.write(bytes.array())
  }

  private def writeBytes(outputStream: OutputStream, bytes: Iterable[Byte]): Unit = {
    outputStream.write(bytes.toArray)
  }

}
