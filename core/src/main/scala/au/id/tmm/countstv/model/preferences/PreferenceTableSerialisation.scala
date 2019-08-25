package au.id.tmm.countstv.model.preferences

import java.io.OutputStream
import java.nio.ByteBuffer
import java.security.{DigestOutputStream, MessageDigest}
import java.util.zip.GZIPOutputStream

import scala.collection.immutable.ArraySeq

private[model] object PreferenceTableSerialisation {

  def serialiseAndCompress[C](preferenceTable: PreferenceTable[C], rawOutputStream: OutputStream): Unit =
    try {
      val outputStream = new GZIPOutputStream(rawOutputStream, true)
      serialise(preferenceTable, outputStream)
    } finally {
      rawOutputStream.close()
    }

  def serialise[C](preferenceTable: PreferenceTable[C], rawOutputStream: OutputStream): Unit = {

    val digest = MessageDigest.getInstance(messageDigestAlgorithm)
    val outputStream = new DigestOutputStream(rawOutputStream, digest)

    writeBytes(outputStream, magicWord)

    writeInts(outputStream,
      ArraySeq(
        serialisationVerson,
        preferenceTable.getTotalNumPapers,
        preferenceTable.getCandidateLookup.length,
      ),
    )

    writeInt(outputStream, preferenceTable.getLength)

    for (i <- 0 until preferenceTable.getLength) {
      writeInt(outputStream, preferenceTable.getRowPaperCounts()(i))
      writeInt(outputStream, preferenceTable.getPreferenceArrays()(i).length)
      writeShorts(outputStream, ArraySeq.unsafeWrapArray(preferenceTable.getPreferenceArrays()(i)))
    }

    val messageDigest = digest.digest()

    outputStream.write(messageDigest)
    outputStream.flush()
  }

  private def writeInt(outputStream: OutputStream, int: Int): Unit = writeInts(outputStream, ArraySeq(int))

  private def writeInts(outputStream: OutputStream, ints: ArraySeq[Int]): Unit = {
    val bytes = ByteBuffer.allocate(ints.size * Integer.BYTES)

    ints.foreach { int =>
      assert(int >= 0)
      bytes.putInt(int)
    }

    outputStream.write(bytes.array())
  }

  private def writeShorts(outputStream: OutputStream, shorts: ArraySeq[Short]): Unit = {
    val bytes = ByteBuffer.allocate(shorts.size * java.lang.Short.BYTES)

    shorts.foreach { short =>
      assert(short >= 0)
      bytes.putShort(short)
    }

    outputStream.write(bytes.array())
  }

  private def writeBytes(outputStream: OutputStream, bytes: ArraySeq[Byte]): Unit = {
    outputStream.write(bytes.unsafeArray.asInstanceOf[Array[Byte]])
  }

}
